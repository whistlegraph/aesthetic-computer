# Shared by lith and session-server. No work happens merely by sourcing it.
function prepare_private_products --argument-names root key host remote node_bin revision
    set -l local_stage (mktemp -d)
    # The manifest and installer come from the revision being deployed, not
    # this checkout, which may be behind it.
    if not git -C "$root" show "$revision:private-products.json" >"$local_stage/private-products.json"
        or not git -C "$root" show "$revision:toolchain/private-products.mjs" >"$local_stage/private-products.mjs"
        rm -rf "$local_stage"
        return 1
    end
    set -l manifest "AC_PRODUCTS_MANIFEST=$local_stage/private-products.json"
    if not env $manifest node "$root/toolchain/private-products.mjs" release-check "$revision"
        or not env $manifest node "$root/toolchain/private-products.mjs" pack "$local_stage/products.tgz"
        rm -rf "$local_stage"
        return 1
    end
    set -g PRODUCT_REMOTE_STAGE (ssh -i "$key" "$host" 'mktemp -d /tmp/ac-products.XXXXXXXX')
    if not string match -qr '^/tmp/ac-products\.[A-Za-z0-9]+$' -- "$PRODUCT_REMOTE_STAGE"
        rm -rf "$local_stage"
        return 1
    end
    if not scp -q -i "$key" "$local_stage/products.tgz" "$local_stage/private-products.mjs" "$local_stage/private-products.json" "$host:$PRODUCT_REMOTE_STAGE/"
        rm -rf "$local_stage"
        return 1
    end
    rm -rf "$local_stage"
    set -g PRODUCT_RUN "env AC_CORE_ROOT="(string escape -- "$remote")" AC_PRODUCTS_MANIFEST=$PRODUCT_REMOTE_STAGE/private-products.json "(string escape -- "$node_bin")" $PRODUCT_REMOTE_STAGE/private-products.mjs"
    ssh -i "$key" "$host" "mkdir $PRODUCT_REMOTE_STAGE/incoming && tar -xzf $PRODUCT_REMOTE_STAGE/products.tgz -C $PRODUCT_REMOTE_STAGE/incoming && $PRODUCT_RUN snapshot $PRODUCT_REMOTE_STAGE/previous"
end
