using System;
using System.Collections.Generic;
using System.Linq;
using System.Windows.Forms;

using OxidePatcher.Patching;
using OxidePatcher.Views;

using Mono.Cecil;
using Mono.Cecil.Cil;

namespace OxidePatcher.Hooks
{
    public enum ReturnBehavior { Continue, ExitWhenValidType, ModifyRefArg, UseArgumentString }

    public enum ArgumentBehavior { None, JustThis, JustParams, All, UseArgumentString }

    /// <summary>
    /// A simple hook that injects at a certain point in the method, with a few options for handling arguments and return values
    /// </summary>
    [HookType("Simple", Default = true)]
    public class Simple : Hook
    {
        /// <summary>
        /// Gets or sets the instruction index to inject the hook call at
        /// </summary>
        public int InjectionIndex { get; set; }

        /// <summary>
        /// Gets or sets the return behavior
        /// </summary>
        public ReturnBehavior ReturnBehavior { get; set; }

        /// <summary>
        /// Gets or sets the argument behavior
        /// </summary>
        public ArgumentBehavior ArgumentBehavior { get; set; }

        /// <summary>
        /// Gets or sets the argument string
        /// </summary>
        public string ArgumentString { get; set; }

        /// <summary>
        /// Represents info about an argument to pass to the hook
        /// </summary>
        private abstract class ArgInfo
        {
            /// <summary>
            /// Gets the name of the argument
            /// </summary>
            public string Name { get; private set; }

            /// <summary>
            /// Gets the type of the argument
            /// </summary>
            public abstract TypeReference Type { get; }

            /// <summary>
            /// Initialises a new instance of the ArgInfo class
            /// </summary>
            /// <param name="name"></param>
            protected ArgInfo(string name)
            {
                Name = name;
            }

            /// <summary>
            /// Inserts instructions that loads this argument onto the stack
            /// </summary>
            /// <param name="weaver"></param>
            public abstract void LoadToStack(ILWeaver weaver);
        }

        /// <summary>
        /// Represents info about an argument backed by a parameter to pass to the hook
        /// </summary>
        private sealed class ArgInfoParam : ArgInfo
        {
            /// <summary>
            /// Gets the parameter that backs this argument
            /// </summary>
            public ParameterDefinition ParamDef { get; private set; }

            /// <summary>
            /// Gets the type of the argument
            /// </summary>
            public override TypeReference Type { get { return ParamDef.ParameterType; } }

            /// <summary>
            /// Initialises a new instance of the ArgInfoParam class
            /// </summary>
            /// <param name="name"></param>
            /// <param name="pDef"></param>
            public ArgInfoParam(string name, ParameterDefinition pDef)
                : base(name)
            {
                ParamDef = pDef;
            }

            /// <summary>
            /// Inserts instructions that loads this argument onto the stack
            /// </summary>
            /// <param name="weaver"></param>
            public override void LoadToStack(ILWeaver weaver)
            {
                weaver.Add(ILWeaver.Ldarg(ParamDef));
            }
        }

        /// <summary>
        /// Represents info about an argument backed by a variable to pass to the hook
        /// </summary>
        private sealed class ArgInfoVar : ArgInfo
        {
            /// <summary>
            /// Gets the parameter that backs this argument
            /// </summary>
            public VariableDefinition VarDef { get; private set; }

            /// <summary>
            /// Gets the type of the argument
            /// </summary>
            public override TypeReference Type { get { return VarDef.VariableType; } }

            /// <summary>
            /// Initialises a new instance of the ArgInfoParam class
            /// </summary>
            /// <param name="name"></param>
            /// <param name="pDef"></param>
            public ArgInfoVar(string name, VariableDefinition vDef)
                : base(name)
            {
                VarDef = vDef;
            }

            /// <summary>
            /// Inserts instructions that loads this argument onto the stack
            /// </summary>
            /// <param name="weaver"></param>
            public override void LoadToStack(ILWeaver weaver)
            {
                weaver.Ldloc(VarDef);
            }
        }

        /// <summary>
        /// Represents info about an argument backed by "this" to pass to the hook
        /// </summary>
        private sealed class ArgInfoThis : ArgInfo
        {
            private TypeReference theType;

            /// <summary>
            /// Gets the type of the argument
            /// </summary>
            public override TypeReference Type { get { return theType; } }

            /// <summary>
            /// Initialises a new instance of the ArgInfoParam class
            /// </summary>
            /// <param name="name"></param>
            /// <param name="pDef"></param>
            public ArgInfoThis(string name, TypeReference type)
                : base(name)
            {
                theType = type;
            }

            /// <summary>
            /// Inserts instructions that loads this argument onto the stack
            /// </summary>
            /// <param name="weaver"></param>
            public override void LoadToStack(ILWeaver weaver)
            {
                weaver.Add(ILWeaver.Ldarg(null));
            }
        }

        private TypeReference Import(MethodDefinition original, PatchContext context, TypeReference toImport)
        {
            if (context.Live)
                return original.Module.Import(toImport);
            else
                return toImport;
        }

        private MethodReference Import(MethodDefinition original, PatchContext context, MethodReference toImport)
        {
            if (context.Live)
                return original.Module.Import(toImport);
            else
                return toImport;
        }

        private FieldReference Import(MethodDefinition original, PatchContext context, FieldReference toImport)
        {
            if (context.Live)
                return original.Module.Import(toImport);
            else
                return toImport;
        }

        public override bool ApplyPatch(MethodDefinition original, ILWeaver weaver, PatchContext context)
        {
            // Get the base type
            TypeDefinition baseTypeDef = original.DeclaringType;

            // Generate the arg type
            string retValue = null;
            ArgInfo[] toPass = GenerateArgInfos(original, out retValue);
            TypeReference argType;
            if (toPass.Length == 0)
            {
                argType = Import(original, context, context.OxideAssembly.MainModule.Types
                    .Single((t) => t.FullName == "Oxide.Core.HookSystem.NoArg"));
            }
            else
            {
                TypeDefinition argTypeDef = CreateHookArgType(context.OxideAssembly, string.Format("{0}Arg", HookName), baseTypeDef, toPass);
                if (context.Live)
                {
                    //baseTypeDef.NestedTypes.Add(argTypeDef);
                    if (!original.Module.Types.Contains(argTypeDef))
                        original.Module.Types.Add(argTypeDef);
                }
                argType = argTypeDef;
            }

            // Get the potential hook types
            TypeDefinition baseHookType2 = context.OxideAssembly.MainModule.Types
                .Single((t) => t.FullName == "Oxide.Core.HookSystem.Hook`2");
            TypeDefinition baseHookType1 = context.OxideAssembly.MainModule.Types
                .Single((t) => t.FullName == "Oxide.Core.HookSystem.Hook`1");

            // Determine the hook return type
            TypeDefinition chosenHookType;
            TypeReference hookReturnType;
            switch (ReturnBehavior)
            {
                case ReturnBehavior.Continue:
                    // Take no action, so ignore the hook's return value
                    // Actually this will select the returnless hook
                    chosenHookType = baseHookType1;
                    hookReturnType = null;
                    break;
                case ReturnBehavior.ExitWhenValidType:
                    // When set, return this method's return type
                    chosenHookType = baseHookType2;
                    hookReturnType = original.ReturnType;
                    break;
                case ReturnBehavior.ModifyRefArg:
                    // When set, modify the byref argument
                    // TODO: Make this use the arg and not just return type of original
                    chosenHookType = baseHookType2;
                    hookReturnType = original.ReturnType;
                    break;
                case ReturnBehavior.UseArgumentString:
                    if (retValue != null)
                    {
                        // Do what the arg string says
                        chosenHookType = baseHookType2;
                        char src = retValue[0];
                        int val = int.Parse(retValue.Substring(1));
                        if (src == 'l' || src == 'v')
                        {
                            var variable = original.Body.Variables[val];
                            hookReturnType = variable.VariableType;
                        }
                        else if (src == 'a' || src == 'p')
                        {
                            var param = original.Parameters[val];
                            hookReturnType = param.ParameterType;
                        }
                        else
                            hookReturnType = null;
                    }
                    else
                    {
                        // This hook's ReturnBehaviour is misconfigured, but let it slide anyway because we're nice
                        chosenHookType = baseHookType1;
                        hookReturnType = null;
                    }
                    break;
                default:
                    throw new Exception("Unhandled ReturnBehaviour");
            }



            // Generate the field
            TypeReference specialisedHookType;
            if (hookReturnType == null)
                specialisedHookType = chosenHookType.MakeGenericInstanceType(argType);
            else
                specialisedHookType = chosenHookType.MakeGenericInstanceType(hookReturnType, argType);
            FieldDefinition hookField = new FieldDefinition(HookName, FieldAttributes.Public | FieldAttributes.InitOnly | FieldAttributes.Static, Import(original, context, specialisedHookType));
            if (context.Live) baseTypeDef.Fields.Add(hookField);
            MethodReference genericHookTypeCtor = specialisedHookType.Resolve().Methods.Single((m) => m.IsConstructor);
            MethodReference specialisedHookTypeCtor;
            if (hookReturnType == null)
                specialisedHookTypeCtor = genericHookTypeCtor.MakeHostInstanceGeneric(argType);
            else
                specialisedHookTypeCtor = genericHookTypeCtor.MakeHostInstanceGeneric(hookReturnType, argType);

            // Get hook call method
            MethodReference genericHookCallMethod = chosenHookType.Resolve().Methods.Single((m) => m.Name == "Call");
            MethodReference specialisedHookCallMethod = Import(original, context, genericHookCallMethod.MakeHostInstanceGeneric(argType));

            // Modify all constructors to initialise the field
            if (context.Live)
            {
                int ctorCount = 0;
                foreach (MethodDefinition ctorDef in baseTypeDef.Methods.Where((m) => m.IsConstructor && m.IsStatic))
                {
                    ILWeaver ctorWeaver = new ILWeaver(ctorDef.Body);
                    ctorWeaver.Pointer = 0;
                    if (hookReturnType != null)
                        ctorWeaver.Add(ILWeaver.Ldc_I4_n(1)); // TODO: Use enum properly
                    ctorWeaver.Add(Instruction.Create(OpCodes.Ldstr, HookName));
                    ctorWeaver.Add(Instruction.Create(OpCodes.Newobj, Import(original, context, specialisedHookTypeCtor)));
                    ctorWeaver.Add(Instruction.Create(OpCodes.Stsfld, hookField));
                    ctorWeaver.Apply(ctorDef.Body);
                    ctorCount++;
                }
                if (ctorCount == 0)
                {
                    // Emit a new static constructor
                    MethodDefinition ctorDef = new MethodDefinition(".cctor",
                        MethodAttributes.Private |
                        MethodAttributes.Static |
                        MethodAttributes.ReuseSlot |
                        MethodAttributes.HideBySig |
                        MethodAttributes.SpecialName |
                        MethodAttributes.RTSpecialName,
                        original.Module.TypeSystem.Void);
                    
                    ILWeaver ctorIL = new ILWeaver();
                    if (hookReturnType != null)
                        ctorIL.Add(ILWeaver.Ldc_I4_n(1)); // TODO: Use enum properly
                    ctorIL.Add(Instruction.Create(OpCodes.Ldstr, HookName));
                    ctorIL.Add(Instruction.Create(OpCodes.Newobj, Import(original, context, specialisedHookTypeCtor)));
                    ctorIL.Add(Instruction.Create(OpCodes.Stsfld, hookField));
                    
                    ctorIL.Add(Instruction.Create(OpCodes.Ret));

                    MethodBody ctorBody = new MethodBody(ctorDef);
                    ctorIL.Apply(ctorBody);
                    ctorDef.Body = ctorBody;
                    if (context.Live) baseTypeDef.Methods.Add(ctorDef);
                }
            }

            // Start injecting where requested
            weaver.Pointer = InjectionIndex;

            // Get the existing instruction we're going to inject behind
            Instruction existing;
            try
            {
                existing = weaver.Instructions[weaver.Pointer];
            }
            catch (ArgumentOutOfRangeException)
            {
                if (!context.Console)
                {
                    MessageBox.Show(string.Format("The injection index specified for {0} is invalid!", this.Name), "Invalid Index", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
                return false;
            }

            // Get HookReturnValue and specialise as needed
            TypeReference genericHookReturnValue = new TypeReference("Oxide.Core.HookSystem", "HookReturnValue`1", context.OxideAssembly.MainModule, chosenHookType.Scope);
            genericHookReturnValue = Import(original, context, genericHookReturnValue);
            TypeReference specialisedHookReturnValue = null;
            FieldReference specialisedHookReturnValue_hasValue = null, specialisedHookReturnValue_value = null;
            if (hookReturnType != null)
            {
                specialisedHookReturnValue = genericHookReturnValue.MakeGenericInstanceType(hookReturnType);

                specialisedHookReturnValue_hasValue = Import(original, context, new FieldReference("HasValue", original.Module.TypeSystem.Boolean, specialisedHookReturnValue));
                specialisedHookReturnValue_value = Import(original, context, new FieldReference("Value", hookReturnType, specialisedHookReturnValue));
            }


            // Add a variable for the return value
            VariableDefinition hookReturnVariable = null;
            if (hookReturnType != null)
            {
                hookReturnVariable = weaver.AddVariable(specialisedHookReturnValue, "hookReturn");
            }

            // Load the event object
            var firstInjected = weaver.Add(ILWeaver.Ldarg(null));
            weaver.Add(Instruction.Create(OpCodes.Ldfld, hookField));

            // Do we have args to pass?
            if (toPass.Length == 0)
            {
                // Add variable for the arg
                VariableDefinition hookArgVariable = weaver.AddVariable(argType, "hookArg");

                // Load empty NoArg
                weaver.Add(Instruction.Create(OpCodes.Ldloca_S, hookArgVariable));
                weaver.Add(Instruction.Create(OpCodes.Initobj, argType));
                weaver.Ldloc(hookArgVariable);
            }
            else
            {
                // Load all args to stack
                for (int i = 0; i < toPass.Length; i++)
                {
                    toPass[i].LoadToStack(weaver);
                }

                // Initialise the arg struct
                var ctor = Import(original, context, argType.Resolve().Methods.Single((m) => m.IsConstructor));
                weaver.Add(Instruction.Create(OpCodes.Newobj, ctor));
            }

            // Load the arg and call
            weaver.Add(Instruction.Create(OpCodes.Callvirt, specialisedHookCallMethod));

            // Do we have a return value to process?
            if (hookReturnType != null)
            {
                // Store in the return variable
                weaver.Stloc(hookReturnVariable);

                // Read HasValue
                weaver.Ldloc(hookReturnVariable);
                var ins = weaver.Add(Instruction.Create(OpCodes.Ldfld, specialisedHookReturnValue_hasValue));

                // If it's false, always jump to the next non-injected instruction
                weaver.Add(Instruction.Create(OpCodes.Brfalse_S, ins.Next));

                // Process return type
                switch (ReturnBehavior)
                {
                    case ReturnBehavior.ExitWhenValidType:
                        // Read value
                        //weaver.Ldloc(hookReturnVariable);
                        //weaver.Add(Instruction.Create(OpCodes.Ldfld, specialisedHookReturnValue_value));

                        // Jump to the return
                        //weaver.Add(Instruction.Create(OpCodes.Ret));
                        weaver.Add(Instruction.Create(OpCodes.Nop));
                        break;
                    case ReturnBehavior.Continue:
                        // Shouldn't reach here
                        weaver.Add(Instruction.Create(OpCodes.Nop));
                        break;
                    case ReturnBehavior.ModifyRefArg:
                        // TODO: This
                        weaver.Add(Instruction.Create(OpCodes.Nop));
                        break;
                    case ReturnBehavior.UseArgumentString:
                        // Read value
                        weaver.Ldloc(hookReturnVariable);
                        weaver.Add(Instruction.Create(OpCodes.Ldfld, specialisedHookReturnValue_value));

                        // Store in whatever the arg string says to
                        char src = retValue[0];
                        int val = int.Parse(retValue.Substring(1));
                        if (src == 'l' || src == 'v')
                        {
                            var variable = original.Body.Variables[val];
                            weaver.Stloc(variable);
                        }
                        else if (src == 'a' || src == 'p')
                        {
                            var param = original.Parameters[val];
                            weaver.Starg(param);
                        }
                        else
                            weaver.Add(Instruction.Create(OpCodes.Nop));
                        break;
                }
            }

            /*var hookname = weaver.Add(Instruction.Create(OpCodes.Ldstr, HookName));
            if (firstinjected == null) firstinjected = hookname;
            if (argsvar != null)
                weaver.Ldloc(argsvar);
            else
                weaver.Add(Instruction.Create(OpCodes.Ldnull));
            weaver.Add(Instruction.Create(OpCodes.Call, original.Module.Import(callhookmethod)));

            // Deal with the return value
            DealWithReturnValue(original, argsvar, weaver);*/

            // Find all instructions which pointed to the existing and redirect them
            for (int i = 0; i < weaver.Instructions.Count; i++)
            {
                Instruction ins = weaver.Instructions[i];
                if (ins.Operand != null && ins.Operand.Equals(existing))
                {
                    // Check if the instruction lies within our injection range
                    // If it does, it's an instruction we just injected so we don't want to edit it
                    if (i < InjectionIndex || i > weaver.Pointer)
                        ins.Operand = firstInjected;
                }
            }
            return true;
        }

        private ArgInfo[] GenerateArgInfos(MethodDefinition method, out string retValue)
        {
            // Define variables
            ArgInfo[] toPass = null;

            // Switch on argument behaviour
            switch (ArgumentBehavior)
            {
                case ArgumentBehavior.None: // No arguments to pass on
                    toPass = new ArgInfo[0];
                    retValue = null;
                    break;
                case ArgumentBehavior.JustParams: // Pass on all the method parameters
                    toPass = method.Parameters

                        .Select((p) => new ArgInfoParam(p.Name, p))
                        .ToArray();
                    retValue = null;
                    break;
                case ArgumentBehavior.JustThis: // Just pass on this (parameter 0)
                    toPass = new ArgInfo[] { new ArgInfoThis("Subject", method.DeclaringType) };
                    retValue = null;
                    break;
                case ArgumentBehavior.All:
                    toPass = new ArgInfo[] { new ArgInfoThis("Subject", method.DeclaringType) }
                        .Concat(method.Parameters.Select((p) => new ArgInfoParam(p.Name, p)))
                        .ToArray();
                    retValue = null;
                    break;
                case ArgumentBehavior.UseArgumentString:
                    string[] argsToUse = ParseArgumentString(out retValue);
                    toPass = new ArgInfo[argsToUse.Length];
                    for (int i = 0; i < argsToUse.Length; i++)
                    {
                        string argToUse = argsToUse[i];
                        if (argToUse == "this")
                        {
                            toPass[i] = new ArgInfoThis("Subject", method.DeclaringType);
                        }
                        else
                        {
                            char src = argToUse[0];
                            int val = int.Parse(argToUse.Substring(1));
                            if (src == 'l' || src == 'v')
                            {
                                var variable = method.Body.Variables[val];
                                if (string.IsNullOrEmpty(variable.Name))
                                    toPass[i] = new ArgInfoVar(string.Format("arg_{0}", variable.VariableType.Name), variable);
                                else
                                    toPass[i] = new ArgInfoVar(variable.Name, variable);
                            }
                            else if (src == 'a' || src == 'p')
                            {
                                var param = method.Parameters[val];
                                toPass[i] = new ArgInfoParam(param.Name, param);
                            }
                            else
                                throw new Exception("Invalid argument string or something");
                        }
                    }
                    break;
                default:
                    throw new Exception("Unhandled ArgumentBehaviour");
            }

            // Return
            return toPass;
        }

        private Instruction PushArgsArray(MethodDefinition method, ILWeaver weaver, out VariableDefinition argsvar)
        {
            // Are we going to use arguments?
            if (ArgumentBehavior == Hooks.ArgumentBehavior.None)
            {
                // Push null and we're done
                argsvar = null;
                return null;
            }

            // Create array variable
            Instruction firstInstruction;
            // Are we using the argument string?
            if (ArgumentBehavior == Hooks.ArgumentBehavior.UseArgumentString)
            {
                string retvalue;
                string[] args = ParseArgumentString(out retvalue);
                if (args == null)
                {
                    // Silently fail, but at least produce valid IL
                    argsvar = null;
                    return null;
                }

                // Create the array
                argsvar = weaver.AddVariable(new ArrayType(method.Module.TypeSystem.Object), "args");
                firstInstruction = weaver.Add(ILWeaver.Ldc_I4_n(args.Length));
                weaver.Add(Instruction.Create(OpCodes.Newarr, method.Module.TypeSystem.Object));
                weaver.Stloc(argsvar);

                // Populate it
                for (int i = 0; i < args.Length; i++)
                {
                    weaver.Ldloc(argsvar);
                    string arg = args[i].ToLowerInvariant();
                    weaver.Add(ILWeaver.Ldc_I4_n(i));
                    if (string.IsNullOrEmpty(arg))
                        weaver.Add(Instruction.Create(OpCodes.Ldnull));
                    else if (arg == "this")
                    {
                        if (method.IsStatic)
                            weaver.Add(Instruction.Create(OpCodes.Ldnull));
                        else
                            weaver.Add(ILWeaver.Ldarg(null));
                    }
                    else if (arg[0] == 'p' || arg[0] == 'a')
                    {
                        int index;
                        if (int.TryParse(arg.Substring(1), out index))
                        {
                            ParameterDefinition pdef;
                            
                            /*if (method.IsStatic)
                                pdef = method.Parameters[index];
                            else
                                pdef = method.Parameters[index + 1];*/
                            pdef = method.Parameters[index];

                            weaver.Add(ILWeaver.Ldarg(pdef));
                            if (pdef.ParameterType.IsByReference)
                            {
                                weaver.Add(Instruction.Create(OpCodes.Ldobj, pdef.ParameterType));
                                weaver.Add(Instruction.Create(OpCodes.Box, pdef.ParameterType));
                            }
                            else if (pdef.ParameterType.IsValueType)
                                weaver.Add(Instruction.Create(OpCodes.Box, pdef.ParameterType));
                        }
                        else
                            weaver.Add(Instruction.Create(OpCodes.Ldnull));
                    }
                    else if (arg[0] == 'l' || arg[0] == 'v')
                    {
                        int index;
                        if (int.TryParse(arg.Substring(1), out index))
                        {
                            VariableDefinition vdef = weaver.Variables[index];
                            weaver.Ldloc(vdef);

                            if (vdef.VariableType.IsByReference)
                            {
                                weaver.Add(Instruction.Create(OpCodes.Ldobj, vdef.VariableType));
                                weaver.Add(Instruction.Create(OpCodes.Box, vdef.VariableType));
                            }
                            else if (vdef.VariableType.IsValueType)
                                weaver.Add(Instruction.Create(OpCodes.Box, vdef.VariableType));
                        }
                        else
                            weaver.Add(Instruction.Create(OpCodes.Ldnull));
                    }
                    else
                        weaver.Add(Instruction.Create(OpCodes.Ldnull));
                    
                    
                    weaver.Add(Instruction.Create(OpCodes.Stelem_Ref));
                }
            }
            else
            {
                // Figure out what we're doing
                bool includeargs = ArgumentBehavior == Hooks.ArgumentBehavior.All || ArgumentBehavior == Hooks.ArgumentBehavior.JustParams;
                bool includethis = ArgumentBehavior == Hooks.ArgumentBehavior.All || ArgumentBehavior == Hooks.ArgumentBehavior.JustThis;
                if (method.IsStatic) includethis = false;

                // Work out what arguments we're going to transmit
                List<ParameterDefinition> args = new List<ParameterDefinition>();
                if (includeargs)
                {
                    for (int i = 0; i < method.Parameters.Count; i++)
                    {
                        ParameterDefinition arg = method.Parameters[i];
                        if (!arg.IsOut)
                            args.Add(arg);
                    }
                }

                argsvar = weaver.AddVariable(new ArrayType(method.Module.TypeSystem.Object), "args");

                // Load arg count, create array, store
                if (includethis)
                    firstInstruction = weaver.Add(ILWeaver.Ldc_I4_n(args.Count + 1));
                else
                    firstInstruction = weaver.Add(ILWeaver.Ldc_I4_n(args.Count));
                weaver.Add(Instruction.Create(OpCodes.Newarr, method.Module.TypeSystem.Object));
                weaver.Stloc(argsvar);

                // Include this
                if (includethis)
                {
                    weaver.Ldloc(argsvar);
                    weaver.Add(ILWeaver.Ldc_I4_n(0));
                    weaver.Add(ILWeaver.Ldarg(null));
                    weaver.Add(Instruction.Create(OpCodes.Stelem_Ref));
                }

                // Loop each argument
                for (int i = 0; i < args.Count; i++)
                {
                    // Load array, load index load arg, store in array
                    ParameterDefinition arg = args[i];
                    weaver.Ldloc(argsvar);
                    if (includethis)
                        weaver.Add(ILWeaver.Ldc_I4_n(i + 1));
                    else
                        weaver.Add(ILWeaver.Ldc_I4_n(i));
                    weaver.Add(ILWeaver.Ldarg(args[i]));
                    if (arg.ParameterType.IsByReference)
                    {
                        weaver.Add(Instruction.Create(OpCodes.Ldobj, arg.ParameterType));
                        weaver.Add(Instruction.Create(OpCodes.Box, arg.ParameterType));
                    }
                    else if (arg.ParameterType.IsValueType)
                        weaver.Add(Instruction.Create(OpCodes.Box, arg.ParameterType));
                    weaver.Add(Instruction.Create(OpCodes.Stelem_Ref));
                }
            }
            return firstInstruction;
        }

        private void DealWithReturnValue(MethodDefinition method, VariableDefinition argsvar, ILWeaver weaver)
        {
            // What return behavior do we use?
            switch (ReturnBehavior)
            {
                case Hooks.ReturnBehavior.Continue:
                    // Just discard the return value
                    weaver.Add(Instruction.Create(OpCodes.Pop));
                    break;
                case Hooks.ReturnBehavior.ExitWhenValidType:
                    // Is there a return value or not?
                    if (method.ReturnType.FullName == "System.Void")
                    {
                        // If the hook returned something that was non-null, return
                        Instruction i = weaver.Add(Instruction.Create(OpCodes.Ldnull));
                        weaver.Add(Instruction.Create(OpCodes.Beq_S, i.Next));
                        weaver.Add(Instruction.Create(OpCodes.Ret));
                    }
                    else
                    {
                        // Create variable
                        VariableDefinition returnvar = weaver.AddVariable(method.Module.TypeSystem.Object, "returnvar");

                        // Store the return value in it
                        weaver.Stloc(returnvar);
                        weaver.Ldloc(returnvar);

                        // If it's non-null and matches the return type, return it - else continue
                        weaver.Add(Instruction.Create(OpCodes.Isinst, method.ReturnType));
                        Instruction i = weaver.Add(Instruction.Create(OpCodes.Ldnull));
                        weaver.Add(Instruction.Create(OpCodes.Beq_S, i.Next));
                        weaver.Ldloc(returnvar);
                        weaver.Add(Instruction.Create(OpCodes.Unbox_Any, method.ReturnType));
                        weaver.Add(Instruction.Create(OpCodes.Ret));
                    }
                    break;
                case Hooks.ReturnBehavior.ModifyRefArg:
                    string wayne;
                    var args = ParseArgumentString(out wayne);
                    if (args == null)
                    {
                        break;
                    }
                    for (int i = 0; i < args.Length; i++)
                    {
                        string arg = args[i].ToLowerInvariant();
                        if (arg[0] == 'p' || arg[0] == 'a')
                        {
                            int index;
                            if (int.TryParse(arg.Substring(1), out index))
                            {
                                var pdef = method.Parameters[index];
                                if (pdef.ParameterType.IsValueType)
                                {
                                    weaver.Ldloc(argsvar);
                                    weaver.Add(ILWeaver.Ldc_I4_n(i));
                                    weaver.Add(Instruction.Create(OpCodes.Ldelem_Ref));
                                    weaver.Add(Instruction.Create(OpCodes.Unbox_Any, pdef.ParameterType));
                                    weaver.Starg(pdef);
                                }
                            }
                        }
                        else if (arg[0] == 'l' || arg[0] == 'v')
                        {
                            int index;
                            if (int.TryParse(arg.Substring(1), out index))
                            {
                                var vdef = weaver.Variables[index];
                                if (vdef.VariableType.IsValueType)
                                {
                                    weaver.Ldloc(argsvar);
                                    weaver.Add(ILWeaver.Ldc_I4_n(i));
                                    weaver.Add(Instruction.Create(OpCodes.Ldelem_Ref));
                                    weaver.Add(Instruction.Create(OpCodes.Unbox_Any, vdef.VariableType));
                                    weaver.Stloc(vdef);
                                }
                            }
                        }
                    }
                    weaver.Add(Instruction.Create(OpCodes.Pop));
                    break;
                case Hooks.ReturnBehavior.UseArgumentString:
                    // Deal with it according to the retvalue of the arg string
                    string retvalue;
                    ParseArgumentString(out retvalue);
                    if (!string.IsNullOrEmpty(retvalue))
                    {
                        if (retvalue[0] == 'l' && retvalue.Length > 1)
                        {
                            int localindex;
                            if (int.TryParse(retvalue.Substring(1), out localindex))
                            {
                                // Create variable and get the target variable
                                VariableDefinition returnvar = weaver.AddVariable(method.Module.TypeSystem.Object, "returnvar");
                                VariableDefinition targetvar = weaver.Variables[localindex];

                                // Store the return value in it
                                weaver.Stloc(returnvar);
                                weaver.Ldloc(returnvar);

                                // If it's non-null and matches the variable type, store it in the target variable
                                weaver.Add(Instruction.Create(OpCodes.Isinst, targetvar.VariableType));
                                Instruction i = weaver.Add(Instruction.Create(OpCodes.Ldnull));
                                weaver.Add(Instruction.Create(OpCodes.Beq_S, i.Next));
                                weaver.Ldloc(returnvar);
                                weaver.Add(Instruction.Create(OpCodes.Unbox_Any, targetvar.VariableType));
                                weaver.Stloc(targetvar);

                                // Handled
                                return;
                            }
                        }
                        else if (retvalue == "ret" || retvalue == "return")
                        {
                            // Create variable
                            VariableDefinition returnvar = weaver.AddVariable(method.Module.TypeSystem.Object, "returnvar");

                            // Store the return value in it
                            weaver.Stloc(returnvar);
                            weaver.Ldloc(returnvar);

                            // If it's non-null and matches the return type, return it - else continue
                            weaver.Add(Instruction.Create(OpCodes.Isinst, method.ReturnType));
                            Instruction i = weaver.Add(Instruction.Create(OpCodes.Ldnull));
                            weaver.Add(Instruction.Create(OpCodes.Beq_S, i.Next));
                            weaver.Ldloc(returnvar);
                            weaver.Add(Instruction.Create(OpCodes.Unbox_Any, method.ReturnType));
                            weaver.Add(Instruction.Create(OpCodes.Ret));

                            // Handled
                            return;
                        }
                    }

                    // Not handled
                    weaver.Add(Instruction.Create(OpCodes.Pop));
                    break;
            }
            
        }

        private string[] ParseArgumentString(out string retvalue)
        {
            // Check arg string for null
            if (string.IsNullOrEmpty(ArgumentString))
            {
                retvalue = null;
                return null;
            }

            // Strip whitespace
            string argstr = new string(ArgumentString.Where((c) => !char.IsWhiteSpace(c)).ToArray());

            // Split by return value indicator
            string[] leftright = argstr.Split(new string[1] { "=>" }, StringSplitOptions.RemoveEmptyEntries);
            if (leftright.Length == 0)
            {
                retvalue = null;
                return null;
            }

            // Split by comma
            string[] args = leftright[0].Split(',');

            // Set the return value
            if (leftright.Length > 1)
                retvalue = leftright[1];
            else
                retvalue = null;

            // Return
            return args;
        }

        private bool CheckHookArgTypeMatch(TypeDefinition argType, ArgInfo[] arguments)
        {
            if (argType.Fields.Count != arguments.Length) return false;
            for (int i = 0; i < arguments.Length; i++)
            {
                TypeReference typeRef = arguments[i].Type;
                if (typeRef.FullName != argType.Fields[i].FieldType.FullName)
                    return false;
            }
            return true;
        }

        private TypeDefinition CreateHookArgType(AssemblyDefinition oxideAssembly, string name, TypeDefinition parent, ArgInfo[] arguments)
        {
            // Check for existing type
            TypeDefinition existingArgType = parent.Module.Types.SingleOrDefault((t) => t.Name == name && t.Namespace == "Oxide.HookArgs");
            if (existingArgType != null)
            {
                // What do we do?
                // If it matches up to our definition, just use it
                // Otherwise, change the name to have some number at the end
                if (CheckHookArgTypeMatch(existingArgType, arguments)) return existingArgType;

                int i = 1;
                while (parent.Module.Types.Any((t) => t.Name == string.Format("{0}{1}", name, i) && t.Namespace == "Oxide.HookArgs")) i++;
                name = string.Format("{0}{1}", name, i);
            }

            // Get base type
            TypeReference valueTypeRef = parent.Module.Import(new TypeReference("System", "ValueType", parent.Module.TypeSystem.Object.Module, parent.Module.TypeSystem.Object.Scope));

            // Create the new type definition
            TypeDefinition argType = new TypeDefinition("Oxide.HookArgs", name,
                TypeAttributes.Class |
                TypeAttributes.Public |
                TypeAttributes.AnsiClass |
                TypeAttributes.SequentialLayout |
                TypeAttributes.AnsiClass |
                TypeAttributes.BeforeFieldInit |
                TypeAttributes.Sealed,
                valueTypeRef);

            // Add the arguments as fields
            for (int i = 0; i < arguments.Length; i++)
            {
                ArgInfo arg = arguments[i];
                FieldDefinition fieldDef = new FieldDefinition(arg.Name, FieldAttributes.Public | FieldAttributes.InitOnly, arg.Type);
                argType.Fields.Add(fieldDef);
            }

            // Add a constructor
            MethodDefinition ctorDef = new MethodDefinition(".ctor",
                MethodAttributes.Public |
                MethodAttributes.ReuseSlot |
                MethodAttributes.HideBySig |
                MethodAttributes.SpecialName |
                MethodAttributes.RTSpecialName,
                oxideAssembly.MainModule.TypeSystem.Void);
            ILWeaver ctorIL = new ILWeaver();
            for (int i = 0; i < arguments.Length; i++)
            {
                ParameterDefinition pDef = new ParameterDefinition(arguments[i].Name, ParameterAttributes.None, parent.Module.Import(arguments[i].Type));
                ctorDef.Parameters.Add(pDef);
                ctorIL.Add(Instruction.Create(OpCodes.Ldarg_0));
                ctorIL.Add(ILWeaver.Ldarg(pDef));
                ctorIL.Add(Instruction.Create(OpCodes.Stfld, argType.Fields[i]));
            }
            ctorIL.Add(Instruction.Create(OpCodes.Ret));
            MethodBody ctorBody = new MethodBody(ctorDef);
            ctorDef.Body = ctorBody;
            ctorIL.Apply(ctorBody);
            argType.Methods.Add(ctorDef);

            // Done
            return argType;
        }


        public override HookSettingsControl CreateSettingsView()
        {
            SimpleHookSettingsControl control = new SimpleHookSettingsControl();
            control.Hook = this;
            return control;
        }
    }
}
