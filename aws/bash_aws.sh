
function ec2-desc() {
    local filter="Name=key-name,Values=$1"
    local query="Reservations[].Instances[].$2"
    aws ec2 describe-instances \
        --filters $filter \
        --query $query \
        | sed -n 2p \
        | tr -d '"' \
        | tr -d ' '
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


ssh -i ~/.aws/cloud-dev.pem akhong@$(cloud-dev-dns)
