// Reference: https://www.figma.com/plugin-docs/api/api-reference/
// TODO: At the moment audioWorklet along with microphone and camera are not working.
//         - Perhaps I could reach out to FigJam to fix some of these?
// TODO: 1. Submit widget
//          - https://help.figma.com/hc/en-us/articles/360039958914
//       2. Wire up widget so that it can have inputs
//          - https://www.figma.com/community/widget/1040013605559486449/Play-Button
var __awaiter =
  (this && this.__awaiter) ||
  function (thisArg, _arguments, P, generator) {
    function adopt(value) {
      return value instanceof P
        ? value
        : new P(function (resolve) {
            resolve(value);
          });
    }
    return new (P || (P = Promise))(function (resolve, reject) {
      function fulfilled(value) {
        try {
          step(generator.next(value));
        } catch (e) {
          reject(e);
        }
      }
      function rejected(value) {
        try {
          step(generator["throw"](value));
        } catch (e) {
          reject(e);
        }
      }
      function step(result) {
        result.done
          ? resolve(result.value)
          : adopt(result.value).then(fulfilled, rejected);
      }
      step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
  };
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
  return figma.widget.h(
    AutoLayout,
    {
      verticalAlignItems: "center",
      spacing: 16,
      padding: 16,
      cornerRadius: 8,
      fill: { r: 0.8, g: 0.8, b: 0.9, a: 1 },
      stroke: { r: 0.7, g: 0.7, b: 0.8, a: 1.0 },
      strokeWidth: 6,
    },
    figma.widget.h(
      Text,
      {
        fontSize: 32,
        fill: { r: 0, g: 0.7, b: 0, a: 1 },
        onClick: () =>
          new Promise((resolve) => {
            // TODO: Read disk property value so that it can be autoselected.
            figma.showUI(__html__, { width: 640, height: 480 });
            // Send image data now.
            const widgetNode = figma.getNodeById(widgetId);
            const allConnectorNodes = figma.currentPage.findAll((node) => {
              return node.type === "CONNECTOR";
            });
            allConnectorNodes.forEach((cn) =>
              __awaiter(this, void 0, void 0, function* () {
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
                const bytes = yield img.getBytesAsync();
                figma.ui.postMessage({ type: "figma-image-input", bytes });
              })
            );
          }),
      },
      "\u25B6"
    ),
    figma.widget.h(
      Text,
      {
        fontSize: 32,
        fill: { r: 0.7, g: 0, b: 0, a: 1 },
        onClick: () => new Promise((resolve) => figma.closePlugin()),
      },
      "\u25A0"
    )
  );
}
widget.register(Widget);
