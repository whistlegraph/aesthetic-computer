// Ticket, 23.10.26.19.33
// This file orchestrates a clientside Stripe pop-up and
// assumes Stripe has been loaded and a proper template
// had been added to the DOM.
// This example originally came from: https://stripe.com/docs/payments/quickstart

export function ticket(from, item) {
  let pubKey;
  if (window.acDEBUG) {
    pubKey =
      from === "sotce"
        ? SOTCE_STRIPE_API_TEST_PUB_KEY
        : STRIPE_API_TEST_PUB_KEY;
  } else {
    pubKey = from === "sotce" ? SOTCE_STRIPE_API_PUB_KEY : STRIPE_API_PUB_KEY;
  }

  const stripe = Stripe(pubKey);
  const items = [{ id: item }]; // Items to buy...

  let elements;

  initialize();
  checkStatus();

  document
    .querySelector("#payment-form")
    .addEventListener("submit", handleSubmit);

  let emailAddress = "";
  // Fetches a payment intent and captures the client secret
  async function initialize() {
    const response = await fetch("/api/ticket?new=true", {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ items, from }),
    });
    const { clientSecret } = await response.json();

    console.log("Secret:", clientSecret);

    const appearance = {
      theme: "stripe",
    };

    elements = stripe.elements({ appearance, clientSecret });

    const linkAuthenticationElement = elements.create("linkAuthentication");
    linkAuthenticationElement.mount("#link-authentication-element");

    linkAuthenticationElement.on("change", (event) => {
      emailAddress = event.value.email;
    });

    const paymentElementOptions = { layout: "tabs" };

    const paymentElement = elements.create("payment", paymentElementOptions);
    paymentElement.mount("#payment-element");
  }

  async function handleSubmit(e) {
    e.preventDefault();
    setLoading(true);

    const { error } = await stripe.confirmPayment({
      elements,
      confirmParams: {
        // Make sure to change this to your payment completion page
        return_url: window.location.href + "?notice=check your email",
        receipt_email: emailAddress,
      },
    });

    // This point will only be reached if there is an immediate error when
    // confirming the payment. Otherwise, your customer will be redirected to
    // your `return_url`. For some payment methods like iDEAL, your customer will
    // be redirected to an intermediate site first to authorize the payment, then
    // redirected to the `return_url`.
    if (error.type === "card_error" || error.type === "validation_error") {
      showMessage(error.message);
    } else {
      showMessage("An unexpected error occurred.");
    }

    setLoading(false);
  }

  // Fetches the payment intent status after payment submission
  async function checkStatus() {
    const clientSecret = new URLSearchParams(window.location.search).get(
      "payment_intent_client_secret",
    );

    if (!clientSecret) {
      return;
    }

    const { paymentIntent } = await stripe.retrievePaymentIntent(clientSecret);

    switch (paymentIntent.status) {
      case "succeeded":
        showMessage("Payment succeeded!");
        break;
      case "processing":
        showMessage("Your payment is processing.");
        break;
      case "requires_payment_method":
        showMessage("Your payment was not successful, please try again.");
        break;
      default:
        showMessage("Something went wrong.");
        break;
    }
  }

  // ------- UI helpers -------

  function showMessage(messageText) {
    const messageContainer = document.querySelector("#payment-message");

    messageContainer.classList.remove("hidden");
    messageContainer.textContent = messageText;

    setTimeout(function () {
      messageContainer.classList.add("hidden");
      messageContainer.textContent = "";
    }, 4000);
  }

  // Show a spinner on payment submission
  function setLoading(isLoading) {
    if (isLoading) {
      // Disable the button and show a spinner
      document.querySelector("#submit").disabled = true;
      document.querySelector("#spinner").classList.remove("hidden");
      document.querySelector("#button-text").classList.add("hidden");
    } else {
      document.querySelector("#submit").disabled = false;
      document.querySelector("#spinner").classList.add("hidden");
      document.querySelector("#button-text").classList.remove("hidden");
    }
  }
}

const STRIPE_API_TEST_PUB_KEY =
  "pk_test_51NkALeD01uz279HJwTfh3Ay4aMyav3eN3RyKcSx3NoTrVM9O9bwpaqkkYhCkzVSqE7TeY9HQA7Jv4BhQi5bMNHPe00I1RYBATb";
const STRIPE_API_PUB_KEY =
  "k_live_51NkALeD01uz279HJU3uJeHItXtyixfjW9hSasxvmqpS1bqFrlJZ3kjzvL8noxk7dGY9IC3vb0ZnRQCPMBOeMOdA000m9hhmZwQ";
const SOTCE_STRIPE_API_TEST_PUB_KEY =
  "pk_test_51MoIChA9SniwoPrC2GknVbg3l1V8go2FUnsVf8EIX4kdgCSBec9QN8sxqYrGn0wBUdppuiXpP2CzvWJHclVBsEZv00BtcJwyNq";
const SOTCE_STRIPE_API_PUB_KEY =
  "pk_live_51MoIChA9SniwoPrCUkckJn1MKBBirbmWOv3ET93KwDwapigm1eon1gURpRunANGgjZshpMh7RFFaZA5ZYlOGFTZz00gFTwStMM";