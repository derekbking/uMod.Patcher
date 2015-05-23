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

        private struct ArgInfo
        {
            public readonly string Name;
            public readonly TypeReference Type;

            public ArgInfo(string name, TypeReference type)
            {
                Name = name;
                Type = type;
            }
        }

        public override bool ApplyPatch(MethodDefinition original, ILWeaver weaver, AssemblyDefinition oxideassembly, bool console)
        {
            // Get the base type
            TypeDefinition baseTypeDef = original.DeclaringType;

            // Generate the arg type
            ArgInfo[] toPass = null;
            switch (ArgumentBehavior)
            {
                case ArgumentBehavior.None:
                    toPass = new ArgInfo[0];
                    break;
                case ArgumentBehavior.JustParams:
                    toPass = original.Parameters
                        .Select((p) => new ArgInfo(p.Name, p.ParameterType))
                        .ToArray();
                    break;
                case ArgumentBehavior.JustThis:
                    toPass = new ArgInfo[] { new ArgInfo("Subject", baseTypeDef) };
                    break;
                case ArgumentBehavior.All:
                    toPass = original.Parameters
                        .Select((p) => new ArgInfo(p.Name, p.ParameterType))
                        .Concat(new ArgInfo[] { new ArgInfo("Subject", baseTypeDef) })
                        .ToArray();
                    break;
                case ArgumentBehavior.UseArgumentString:
                    string retValue;
                    string[] argsToUse = ParseArgumentString(out retValue);
                    toPass = new ArgInfo[argsToUse.Length];
                    for (int i = 0; i < argsToUse.Length; i++)
                    {
                        string argToUse = argsToUse[i];
                        if (argToUse == "this")
                        {
                            toPass[i] = new ArgInfo("Subject", baseTypeDef);
                        }
                        else
                        {
                            char src = argToUse[0];
                            int val = int.Parse(argToUse.Substring(1));
                            if (src == 'l' || src == 'v')
                            {
                                var variable = original.Body.Variables[val];
                                if (string.IsNullOrEmpty(variable.Name))
                                    toPass[i] = new ArgInfo(string.Format("arg_{0}", variable.VariableType.Name), variable.VariableType);
                                else
                                    toPass[i] = new ArgInfo(variable.Name, variable.VariableType);
                            }
                            else if (src == 'a' || src == 'p')
                            {
                                var param = original.Parameters[val];
                                toPass[i] = new ArgInfo(param.Name, param.ParameterType);
                            }
                            else
                                throw new Exception("Invalid argument string or something");
                        }
                    }
                    break;
                default:
                    throw new Exception("Unhandled ArgumentBehaviour");
            }
            TypeDefinition argType = CreateHookArgType(oxideassembly, string.Format("{0}Arg", HookName), baseTypeDef, toPass);
            baseTypeDef.NestedTypes.Add(argType);

            // Generate the hook class member
            TypeDefinition baseHookType = oxideassembly.MainModule.Types
                .Single((t) => t.FullName == "Oxide.Core.HookSystem.Hook`2");

            // Generate the hook member
            GenericInstanceType specialisedHookType = new GenericInstanceType(baseHookType);
            switch (ReturnBehavior)
            {
                case ReturnBehavior.Continue:
                    specialisedHookType.GenericArguments.Add(oxideassembly.MainModule.TypeSystem.Object);
                    break;
                case ReturnBehavior.ExitWhenValidType:
                    specialisedHookType.GenericArguments.Add(original.ReturnType);
                    break;
                case ReturnBehavior.ModifyRefArg:
                    specialisedHookType.GenericArguments.Add(oxideassembly.MainModule.TypeSystem.Object);
                    break;
                case ReturnBehavior.UseArgumentString:
                    specialisedHookType.GenericArguments.Add(oxideassembly.MainModule.TypeSystem.Object);
                    break;
                default:
                    throw new Exception("Unhandled ReturnBehaviour");
            }
            specialisedHookType.GenericArguments.Add(argType);
            FieldDefinition hookField = new FieldDefinition(Name, FieldAttributes.Public | FieldAttributes.InitOnly, original.Module.Import(specialisedHookType));
            baseTypeDef.Fields.Add(hookField);
            MethodReference specialisedHookTypeCtor = original.Module.Import(specialisedHookType.Resolve().Methods.SingleOrDefault((m) => m.IsConstructor));

            // Modify all constructors to initialise the field
            int ctorCount = 0;
            foreach (MethodDefinition ctorDef in baseTypeDef.Methods.Where((m) => m.IsConstructor))
            {
                ILWeaver ctorWeaver = new ILWeaver(ctorDef.Body);
                ctorWeaver.Pointer = 0;
                ctorWeaver.Add(ILWeaver.Ldarg(null));
                ctorWeaver.Add(ILWeaver.Ldc_I4_n(0));
                ctorWeaver.Add(Instruction.Create(OpCodes.Ldstr, Name));
                ctorWeaver.Add(Instruction.Create(OpCodes.Newobj, specialisedHookTypeCtor));
                ctorWeaver.Add(Instruction.Create(OpCodes.Stfld, hookField));
                ctorWeaver.Apply(ctorDef.Body);
                ctorCount++;
            }
            if (ctorCount == 0)
            {
                throw new Exception("Target hook class has no constructors!");
            }

            // Get the call hook method
            MethodDefinition callhookmethod = oxideassembly.MainModule.Types
                .Single((t) => t.FullName == "Oxide.Core.Interface")
                .Methods.Single((m) => m.IsStatic && m.Name == "CallHook");

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
                if (console == false)
                {
                    MessageBox.Show(string.Format("The injection index specified for {0} is invalid!", this.Name), "Invalid Index", MessageBoxButtons.OK, MessageBoxIcon.Error);
                }
                return false;
            }

            // Load the hook name

            // Push the arguments array to the stack and make the call
            VariableDefinition argsvar;
            var firstinjected = PushArgsArray(original, weaver, out argsvar);
            var hookname = weaver.Add(Instruction.Create(OpCodes.Ldstr, HookName));
            if (firstinjected == null) firstinjected = hookname;
            if (argsvar != null)
                weaver.Ldloc(argsvar);
            else
                weaver.Add(Instruction.Create(OpCodes.Ldnull));
            weaver.Add(Instruction.Create(OpCodes.Call, original.Module.Import(callhookmethod)));

            // Deal with the return value
            DealWithReturnValue(original, argsvar, weaver);

            // Find all instructions which pointed to the existing and redirect them
            for (int i = 0; i < weaver.Instructions.Count; i++)
            {
                Instruction ins = weaver.Instructions[i];
                if (ins.Operand != null && ins.Operand.Equals(existing))
                {
                    // Check if the instruction lies within our injection range
                    // If it does, it's an instruction we just injected so we don't want to edit it
                    if (i < InjectionIndex || i > weaver.Pointer)
                        ins.Operand = firstinjected;
                }
            }
            return true;
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

        private TypeDefinition CreateHookArgType(AssemblyDefinition oxideAssembly, string name, TypeDefinition parent, ArgInfo[] arguments)
        {
            // Create the new type definition
            TypeDefinition argType = new TypeDefinition(parent.Namespace, name,
                TypeAttributes.Class |
                TypeAttributes.NestedPublic |
                TypeAttributes.SequentialLayout |
                TypeAttributes.AnsiClass |
                TypeAttributes.BeforeFieldInit |
                TypeAttributes.Sealed,
                new TypeReference("System", "ValueType",
                    oxideAssembly.MainModule.TypeSystem.Object.Module,
                    oxideAssembly.MainModule.TypeSystem.Object.Scope)
                    );

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
                ParameterDefinition pDef = new ParameterDefinition(arguments[i].Name, ParameterAttributes.None, arguments[i].Type);
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
