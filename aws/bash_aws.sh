
function ec2-desc() {
    local filter="Name=key-name,Values=$1"
    local query="Reservations[].Instances[].$2"
    aws ec2 describe-instances \
        --filters $filter \
        --filters "Name=instance-state-name,Values=running" \
        --query $query \
        | sed -n 2p \
        | tr -d '"' \
        | tr -d ' '
}

function create-cloud-dev() {
    # TODO: mirror gcp function
    # TODO: default: t2.micro
    aws ec2 run-instances \
        --image-id ami-0c5199d385b432989 \
        --instance-type $1 \
        --key-name cloud-dev \
        --block-device-mappings "[{\"DeviceName\":\"/dev/sdf\",\"Ebs\":{\"VolumeSize\":$2,\"DeleteOnTermination\":true}}]" \
}

function cloud-dev-instance-id() {
    ec2-desc cloud-dev InstanceId
}

function cloud-dev-dns() {
    ec2-desc cloud-dev PublicDnsName
}

function aws-cloud-dev() {
    ssh -i ~/.aws/cloud-dev.pem ubuntu@$(cloud-dev-dns)
}


function aws-terminate-cloud-dev() {
    aws ec2 terminate-instances --instance-ids $(cloud-dev-instance-id)
}
