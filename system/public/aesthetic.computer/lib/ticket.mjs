// Ticket, 23.10.26.19.33
// This file orchestrates a clientside Stripe pop-up and
// assumes Stripe has been loaded and a proper template
// had been added to the DOM.
// This example originally came from: https://stripe.com/docs/payments/quickstart

export function ticket(from, item, ready) {
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
  let clientSecret;

  initialize();
  // checkStatus();

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

    const body = await response.json();
    clientSecret = body.clientSecret;
    const appearance = { theme: "stripe" };

    elements = stripe.elements({ appearance, clientSecret });

    // Add Payment Request Button
    // const paymentRequest = stripe.paymentRequest({
    //   country: "US",
    //   currency: "usd",
    //   total: {
    //     label: "Botce",
    //     amount: 900, // Example amount in cents
    //   },
    //   //   requestPayerName: true,
    //   requestPayerEmail: true,
    // });

    // const prButton = elements.create("paymentRequestButton", {
    //   paymentRequest: paymentRequest,
    // });

    // // Check if the Payment Request API can make payments
    // paymentRequest.canMakePayment().then(function (result) {
    //   if (result) {
    //     prButton.mount("#payment-request-button");
    //   } else {
    //     document.getElementById("payment-request-button").style.display =
    //       "none";
    //   }
    // });

    // Add an event listener for when a payment method is selected
    // paymentRequest.on("paymentmethod", async (event) => {
    //   // Confirm the payment on the server
    //   emailAddress = event.payerEmail;

    //   const { error, paymentIntent } = await stripe.confirmPayment({
    //     elements,
    //     redirect: "if_required",
    //     confirmParams: { receipt_email: emailAddress },
    //   });

    //   console.log(error, paymentIntent);

    //   if (error) {
    //     // Show error in your UI
    //     showMessage(error.message);
    //   } else {
    //     if (paymentIntent.status === "succeeded") {
    //       // Payment succeeded, handle accordingly
    //       showMessage("Payment succeeded!");
    //       checkTicketStatus(); // Start polling for ticket status...
    //     }
    //   }
    // });

    const linkAuthenticationElement = elements.create("linkAuthentication");
    linkAuthenticationElement.mount("#link-authentication-element");

    linkAuthenticationElement.on("change", (event) => {
      emailAddress = event.value.email;
    });

    const paymentElementOptions = { layout: "tabs" };

    const paymentElement = elements.create("payment", paymentElementOptions);
    paymentElement.mount("#payment-element");
    paymentElement.on("ready", ready);
  }

  function deinitialize() {
    const ticketWrapper = document.getElementById("ticket");
    ticketWrapper?.remove();
  }

  async function handleSubmit(e) {
    e.preventDefault();
    setLoading(true);

    const { error } = await stripe.confirmPayment({
      elements,
      redirect: "if_required",
      confirmParams: { receipt_email: emailAddress },
    });

    if (error) {
      if (error.type === "card_error" || error.type === "validation_error") {
        showMessage(error.message);
      } else {
        showMessage("An unexpected error occurred.");
      }
      setLoading(false);
    } else {
      checkTicketStatus(); // Start polling for ticket status...
    }
  }

  let timeout;

  async function checkTicketStatus() {
    try {
      const response = await fetch(
        `/api/ticket?check=true&pid=${clientSecret.split("_secret_")[0]}`,
      );
      const { ticketed, ticket, piece } = await response.json();
      if (ticketed) {
        setLoading(false);
        deinitialize(); // Remove paywall here.
        // Add the ticket to localStorage (under the hood).
        localStorage.setItem(
          `ticket:${ticket.for}`,
          JSON.stringify({ key: ticket.key, time: new Date() }),
        );
        if (window.acDEBUG) console.log("🎟️ Ticket:", ticket);
        clearTimeout(timeout);
        window.acSEND({ type: "jump", content: { piece } }); // Jump to target.
        window.acSEND({ type: "notice", content: "hello :)" }); // Notify.
      } else {
        // Optionally, you can set a timeout to check again
        timeout = setTimeout(checkTicketStatus, 3000); // Check every 3 seconds
      }
    } catch (error) {
      clearTimeout(timeout);
      console.error("Error checking payment status:", error);
      setLoading(false);
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
