# Startup
# - Create user
# - Give user "AdministratorAccess" policy
# - Create key pair
# - Allows SSH connection in security group

# Configure AWS (Region: ap-southeast-1, Output: json)
aws configure

# EC2
aws ec2 describe-instances

aws ec2 describe-instances \
    --filters "Name=instance-type,Values=t2.micro" \
    --query "Reservations[].Instances[].InstanceId"

aws ec2 describe-instances \
    --filters "Name=instance-type,Values=t2.micro" \
    --query "Reservations[].Instances[].PublicDnsName"

aws ec2 create-key-pair --key-name cloud-dev

aws ec2 describe-key-pairs --key-name cloud-dev

aws ec2 run-instances \
    --image-id ami-0c5199d385b432989 \
    --instance-type t2.micro \
    --key-name cloud-dev

export INSTANCE_ID=i-0ff3125497d11faef
export PUBLIC_DNS=ec2-54-254-186-26.ap-southeast-1.compute.amazonaws.com

aws ec2 get-console-output --instance-id $INSTANCE_ID

ssh -i ~/.aws/cloud-dev.pem ubuntu@$PUBLIC_DNS

aws ec2 terminate-instances --instance-ids $INSTANCE_ID

# TODO: how to add storage?
# TODO: provide a startup script
