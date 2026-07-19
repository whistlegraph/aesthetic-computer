using System;
using Windows.ApplicationModel.Activation;
using Windows.UI.Xaml;
using Windows.UI.Xaml.Controls;

namespace Aesthetic.DynamicShell
{
    sealed partial class App : Application
    {
        internal static string EndpointOverride { get; private set; }
        public App() { InitializeComponent(); }

        protected override void OnLaunched(LaunchActivatedEventArgs e)
        {
            EndpointOverride = ParseEndpoint(e.Arguments);
            ShowShell();
        }

        protected override void OnActivated(IActivatedEventArgs args)
        {
            var protocol = args as ProtocolActivatedEventArgs;
            if (protocol != null) EndpointOverride = ParseEndpoint(protocol.Uri.Query);
            ShowShell();
        }

        void ShowShell()
        {
            var frame = Window.Current.Content as Frame ?? new Frame();
            Window.Current.Content = frame;
            if (frame.Content == null) frame.Navigate(typeof(MainPage));
            else ((MainPage)frame.Content).ApplyEndpointOverride();
            Window.Current.Activate();
        }

        static string ParseEndpoint(string arguments)
        {
            if (string.IsNullOrWhiteSpace(arguments)) return null;
            string value = arguments.TrimStart('?');
            foreach (string item in value.Split('&'))
            {
                string[] pair = item.Split(new[] { '=' }, 2);
                if (pair.Length == 2 && pair[0].Equals("endpoint", StringComparison.OrdinalIgnoreCase))
                    return Uri.UnescapeDataString(pair[1]);
            }
            return null;
        }
    }
}
