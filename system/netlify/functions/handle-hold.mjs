import { connect } from "../../backend/database.mjs";
import { filter } from "../../backend/filter.mjs";
import { handleQuarantined } from "../../backend/account-deletion.mjs";
import { validateHandle } from "../../public/aesthetic.computer/lib/text.mjs";
import { createHandleHoldHandler } from "../../backend/handle-hold.mjs";
export const handler = createHandleHoldHandler({ connect, filter, validateHandle, handleQuarantined });
