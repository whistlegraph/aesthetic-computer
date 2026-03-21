export const amount = {
  val: 1.0
}

export function apply(f32) {
  return f32 * amount.val;
}

// Export a volume object that speaker.mjs expects
export const volume = {
  amount,
  apply
};