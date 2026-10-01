// Reviewed feature names only: no messages, links, recipients or preference values.
export const LAKLOK_FEATURE_VERSION = 1;
export const LAKLOK_PIECES = ["laklok", "laklok-vector"];
export const LAKLOK_FEATURES = Object.freeze({
  laklok_settings_opened: ["raster", "vector"],
  laklok_theme_changed: ["raster", "vector"],
  laklok_filter_changed: ["raster", "vector"],
  laklok_language_changed: ["raster"],
  laklok_mode_switch_requested: ["raster", "vector"],
  laklok_mail_open_requested: ["raster"],
  laklok_radio_play_requested: ["raster"],
  laklok_radio_pause_requested: ["raster"],
  laklok_message_send_requested: ["raster", "vector"],
  laklok_message_edit_requested: ["raster", "vector"],
  laklok_history_older_requested: ["vector"],
  laklok_media_open_requested: ["vector"],
});
export const LAKLOK_ACTIONS = Object.freeze(Object.keys(LAKLOK_FEATURES));
export function laklokAction(api, name) {
  const action = `laklok_${name}`;
  if (LAKLOK_ACTIONS.includes(action)) api.send?.({ type: "account:action", content: { action } });
}
