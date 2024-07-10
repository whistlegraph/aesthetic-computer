// Reference: https://www.figma.com/plugin-docs/api/api-reference/
// TODO: At the moment audioWorklet along with microphone and camera are not working.
//         - Perhaps I could reach out to FigJam to fix some of these?
// TODO: 1. Submit widget
//          - https://help.figma.com/hc/en-us/articles/360039958914
//       2. Wire up widget so that it can have inputs
//          - https://www.figma.com/community/widget/1040013605559486449/Play-Button

const { widget } = figma;
const {
  useEffect,
  usePropertyMenu,
  useSyncedState,
  useWidgetId,
  AutoLayout,
  Text,
} = widget;

function Widget() {
  const widgetId = useWidgetId();

  const [disk, setDisk] = useSyncedState("disk", "prompt");

  const diskOptions = [
    { option: "prompt", label: "Prompt" },
    { option: "spray", label: "Spray" },
    { option: "export", label: "Export" },
  ];

  usePropertyMenu(
    [
      {
        itemType: "dropdown",
        propertyName: "disks",
        tooltip: "Starting Disk",
        selectedOption: disk,
        options: diskOptions,
      },
    ],
    ({ propertyName, propertyValue }) => {
      if (propertyName === "disks") setDisk(propertyValue);
    }
  );

  return (
    <AutoLayout
      verticalAlignItems={"center"}
      spacing={16}
      padding={16}
      cornerRadius={8}
      fill={{ r: 0.8, g: 0.8, b: 0.9, a: 1 }}
      stroke={{ r: 0.7, g: 0.7, b: 0.8, a: 1.0 }}
      strokeWidth={6}
    >
      <Text
        fontSize={32}
        fill={{ r: 0, g: 0.7, b: 0, a: 1 }}
        onClick={() =>
          new Promise((resolve) => {
            // TODO: Read disk property value so that it can be autoselected.

            figma.showUI(__html__);

            // Send image data now.
            const widgetNode = figma.getNodeById(widgetId) as WidgetNode;

            const allConnectorNodes: ConnectorNode[] =
              figma.currentPage.findAll((node) => {
                return node.type === "CONNECTOR";
              });

            allConnectorNodes.forEach(async (cn) => {
              // TODO: If end or start is widgetNode.id
              //       and the type is an image... then process it as
              //       an input (only use the first image).

              console.log(
                "Start:",
                figma.getNodeById(cn.connectorStart.endpointNodeId)
              );

              // TODO Unwrap this whole thing a bit.
              const img = figma.getImageByHash(
                figma.getNodeById(cn.connectorEnd.endpointNodeId).fills[0]
                  .imageHash
              );

              const bytes = await img.getBytesAsync();
              figma.ui.postMessage({ type: "figma-image-input", bytes });
            });
          })
        }
      >
        ▶
      </Text>
      <Text
        fontSize={32}
        fill={{ r: 0.7, g: 0, b: 0, a: 1 }}
        onClick={() => new Promise((resolve) => figma.closePlugin())}
      >
        ■
      </Text>
    </AutoLayout>
  );
}

widget.register(Widget);
