using System;

using Umod.Patcher.Hooks;

namespace Umod.Patcher.Views
{
    public partial class InitUmodHookSettingsControl : HookSettingsControl
    {
        private bool ignorechanges;

        public InitUmodHookSettingsControl()
        {
            InitializeComponent();
        }

        protected override void OnLoad(EventArgs e)
        {
            base.OnLoad(e);

            InitUmod hook = Hook as InitUmod;

            ignorechanges = true;
            injectionindex.Value = hook.InjectionIndex;
            ignorechanges = false;
        }

        private void injectionindex_ValueChanged(object sender, EventArgs e)
        {
            if (ignorechanges)
            {
                return;
            }

            InitUmod hook = Hook as InitUmod;
            hook.InjectionIndex = (int)injectionindex.Value;
            NotifyChanges();
        }
    }
}
