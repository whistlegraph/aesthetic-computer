function stripeId(value) {
  if (typeof value === "string") return value;
  if (value && typeof value.id === "string") return value.id;
  return null;
}

function subscriptionProductId(subscription) {
  return stripeId(subscription?.items?.data?.[0]?.price?.product);
}

function invoiceProductId(invoice) {
  const line = invoice?.lines?.data?.[0];
  return stripeId(
    line?.pricing?.price_details?.product ?? line?.price?.product,
  );
}

export { invoiceProductId, stripeId, subscriptionProductId };
