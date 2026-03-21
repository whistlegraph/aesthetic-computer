#!/usr/bin/env fish

# Source the environment variables from the .devcontainer/envs/devcontainer.env file
if test -f /home/me/aesthetic-computer/.devcontainer/envs/devcontainer.env
    for line in (cat /home/me/aesthetic-computer/.devcontainer/envs/devcontainer.env)
        if string match -q "*=*"
            set key (echo $line | cut -d'=' -f1)
            set value (echo $line | cut -d'=' -f2-)
            set -gx $key $value
        end
    end
else
    echo "Environment file not found: /home/me/aesthetic-computer/.devcontainer/envs/devcontainer.env"
    exit 1
end

# Set the product ID for sotce-net
set PRODUCT_ID "prod_QTDSAhsHGMRp3z"

# Initialize variables
set has_more true
set starting_after ""
set total_amount 0

# Loop until all invoices are retrieved
while test $has_more = "true"
    # Fetch invoices from Stripe
    if test -z $starting_after
        set response (curl -s -G https://api.stripe.com/v1/invoices \
            -u $SOTCE_STRIPE_API_KEY_LIVE: \
            --data-urlencode "limit=100")
    else
        set response (curl -s -G https://api.stripe.com/v1/invoices \
            -u $SOTCE_STRIPE_API_KEY_LIVE: \
            --data-urlencode "limit=100" \
            --data-urlencode "starting_after=$starting_after")
    end

    # Extract relevant data using jq
    set invoices (echo $response | jq -c ".data[]")
    set has_more (echo $response | jq -r ".has_more")
    set starting_after (echo $response | jq -r ".data[-1].id")

    # Sum up the amounts for the specified product
    for invoice in $invoices
        set product_id (echo $invoice | jq -r ".lines.data[0].price.product")
        if test $product_id = $PRODUCT_ID
            set amount_paid (echo $invoice | jq -r ".amount_paid")
            set total_amount (math $total_amount + $amount_paid)
        end
    end
end

# Output the total amount in cents
echo "Total revenue for product $PRODUCT_ID: $total_amount cents"

# Convert to dollars (if applicable)
set total_dollars (math $total_amount / 100)
echo "Total revenue in dollars: $total_dollars"