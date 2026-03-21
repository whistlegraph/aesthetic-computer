# digital ocean spaces notes
# originally pulled from: https://711web.com/how-to-set-an-expiry-policy-on-digital-ocean-space-buckets-cloud-cvit/

# list files in a bucket
`aws s3 ls s3://my-bucket --endpoint=https://endpoint.com`

# get the current policy
`aws s3api get-bucket-lifecycle-configuration --bucket my-bucket --endpoint https://endpoint.com`

# apply policy from a policy.json file in the current directory
`aws s3api put-bucket-lifecycle-configuration --bucket my-bucket --endpoint https://endpoint.com --lifecycle-configuration file://policy.json`

# list wand files
aws s3 ls s3://wand.aesthetic.computer --endpoint=https://sfo3.digitaloceanspaces.com  

# list wand files after a certain date
`aws s3api list-objects-v2 --bucket "wand.aesthetic.computer" --prefix "record-prefix" --query "Contents[?LastModified>='2022-11-19'].{key: Key, date: LastModified}" --endpoint-url "https://sfo3.digitaloceanspaces.com"`