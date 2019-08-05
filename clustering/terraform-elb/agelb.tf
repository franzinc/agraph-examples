

provider "aws" {
  profile    = "default"
  region     = "us-east-1"
}


###### start variables

# availability zones.  You need at least two for the load balancer.
# must be in the region declared just above
variable "azs" {
   type = list(string)
   default = ["us-east-1a", "us-east-1b", "us-east-1c"]
}   


# the project name here is used to name some of the 
# resources created here.  
variable "project" {
   type = string
   default = "ag-elb"
}   


# the name of the ami you build that contains an installed
# agraph and the aws-repl files which control startup and
# shutdown actions
variable "ag-elb-ami" {
   type = string
   default = "ami-078f36ac4542c5754"
}


# the name of the ssh-key defined in aws which we'll use
# on all the instance we create.
variable "ssh-key" {
   type = string
}   

# the port and protocol for accessing webview

variable "agraph-port"  {
   type = number
   default = 10035
}

variable "agraph-protocol" {
    type = string
    default = "HTTP"
}    


# The type of machine used in the controlling instance
# and all instances started by the load balancer
# 
variable "instance-type" {
   type = string
   default = "r5.large"
}   


######## end variables

# the vpc name will be the project name
# be sure that this vpc is not currently
# defined now so that this file can specify
# it without conflict.
# 
resource "aws_vpc" "main" {
   cidr_block = "10.0.0.0/16"
   tags = {
      Name = var.project
   }
}


# subnets must be in different AZ's for the load
# balancer to run.
# We need at least two subnets so that we can
# use at least two availability zones

# We don't need the public_ip here but it's handy
# while debugging the cluster.

resource "aws_subnet" "ag-elb-a" {
    vpc_id = aws_vpc.main.id
    cidr_block = "10.0.0.0/24"
    availability_zone = var.azs[0]
    map_public_ip_on_launch = true
    tags = {
      Name = "${var.project}-a"
   }
}

resource "aws_subnet" "ag-elb-b" {
    vpc_id = aws_vpc.main.id
    cidr_block = "10.0.1.0/24"
    availability_zone = var.azs[1]
    map_public_ip_on_launch = true
    tags = {
      Name = "${var.project}-b"
   }
}

resource "aws_subnet" "ag-elb-c" {
    vpc_id = aws_vpc.main.id
    cidr_block = "10.0.2.0/24"
    availability_zone = var.azs[2]
    map_public_ip_on_launch = true
    tags = {
      Name = "${var.project}-c"
   }
}


resource "aws_internet_gateway" "gw" {
  vpc_id = aws_vpc.main.id

  tags = {
    Name = var.project
  }
}


#
# Initially there's a route routing all traffic
# between instances within our VPC so we don't have
# to specify that.
# We only specify additional routes here.
resource "aws_route_table" "ag-elb" {
  vpc_id = aws_vpc.main.id

  route {
    cidr_block = "0.0.0.0/0"
    gateway_id = aws_internet_gateway.gw.id
  }


  tags = {
    Name = var.project
  }
}


resource "aws_route_table_association" "ag-elb-a" {
  subnet_id      = "${aws_subnet.ag-elb-a.id}"
  route_table_id = "${aws_route_table.ag-elb.id}"
}

resource "aws_route_table_association" "ag-elb-b" {
  subnet_id      = "${aws_subnet.ag-elb-b.id}"
  route_table_id = "${aws_route_table.ag-elb.id}"
}

resource "aws_route_table_association" "ag-elb-cc" {
  subnet_id      = "${aws_subnet.ag-elb-c.id}"
  route_table_id = "${aws_route_table.ag-elb.id}"
}


#
# the security group restricts the traffic to
# and from our VPC.   
resource "aws_security_group" "ag-elb" {
    name = "ag-elb"
    description = "ssh and web access from anywhere"
    vpc_id = aws_vpc.main.id

    # ssh from anywhere
    ingress {
       from_port = 22
       to_port = 22
       protocol = "tcp"
       cidr_blocks =["0.0.0.0/0"]
    }

    # Access to webview from anywhere on the internet
    # This is dangerous.  You'll likely want to
    # change the cidr_blocks to specify the addresses
    # of the expected clients of agraph.
    ingress {
       from_port = var.agraph-port
       to_port = var.agraph-port
       protocol = "tcp"
       cidr_blocks =["0.0.0.0/0"]
    }

    # ec2 instances can accept tcp connections on any
    # port from any instance in the VPC
    ingress {
       from_port = 0
       to_port = 65535
       protocol = "tcp"
       cidr_blocks =["10.0.0.0/16"]
    }

    # no restriction on outgoing packets.  We do all
    # the control on incoming packets.
    egress {
       from_port = 0
       to_port = 0
       protocol = "-1"
       cidr_blocks =["0.0.0.0/0"]
    }
   
}


# we start up an ec2 instance to serve as the
# controlling instance of the cluster.
# The controlling instance must have IP
# address 10.0.0.10 (or else you must change
# the value in  vars.sh in the aws-repl code)
# Note that the private_ip, subnet_id and
# availability_zone are related and must be
# consistently defined.
# You'll want to choose the root_block_size
# based on how big your database might grow plus
# enough for doing a backup of the database.
     
resource "aws_instance" "controlling" {
    ami = var.ag-elb-ami
    instance_type = var.instance-type
    availability_zone = var.azs[0]
    private_ip = "10.0.0.10"
    subnet_id = aws_subnet.ag-elb-a.id
    vpc_security_group_ids = [aws_security_group.ag-elb.id]
    key_name = var.ssh-key

    root_block_device {
       volume_size = 30
    }

    tags = {
        Name = "controlling"
    }
}


# The target group specifies a group of instances that will
# receive the HTTP messages that are sent to the load balancer.
# Normally you would list the instances in the target group
# but we'll use an auto scaling group instead and it will
# create and destroy the instances on demand.
resource "aws_lb_target_group" "ag-elb" {
   name = "ag-elb"
   port = var.agraph-port
   protocol = var.agraph-protocol
   vpc_id = aws_vpc.main.id
   health_check {
      path = "/version"
      port = var.agraph-port
      protocol = var.agraph-protocol
   }
   stickiness {
      type = "lb_cookie"
      # how long should a session remain valid, in seconds
      # 432000 is 5 days
      cookie_duration = 432000
      enabled = true
   }
}


# the load balancer will receive and distribute HTTP commands
# to instances in the cluster.  It won't distribute to
# the controlling instance as that instance wasn't created
# by the auto scaling group.
resource "aws_lb" "ag-elb" {
    name = "ag-elb"
    internal = false
    load_balancer_type = "application"
    security_groups = [aws_security_group.ag-elb.id]
    subnets = [aws_subnet.ag-elb-a.id, aws_subnet.ag-elb-b.id, aws_subnet.ag-elb-c.id ]
}


# The load balancer listens on this port and forwards message
# to the named target group
resource "aws_lb_listener" "ag-elb" {
   load_balancer_arn = aws_lb.ag-elb.arn
   port = var.agraph-port
   protocol = var.agraph-protocol

   default_action {
       type = "forward"
       target_group_arn = aws_lb_target_group.ag-elb.arn
   }
}


# when the auto scaling group needs to start an instance
# it uses this configuration to specify the paramters
# of the instance
# 
resource "aws_launch_configuration" "ag-elb" {
   name_prefix = "ag-elb"
   image_id = var.ag-elb-ami
   instance_type = var.instance-type
   security_groups = [aws_security_group.ag-elb.id]
   key_name = var.ssh-key

   root_block_device {
       volume_size = 30
    }       

   lifecycle {
     create_before_destroy = true
   }
}  



# the autoscaing group creates and destroys instances in order
# to achieve the desired_capacity value
# You can adjust the desired capacity by editing this file and
# applying it with terraform, or you can use the aws web interface
# as well.
# It's possible to have the autoscaling group react to the
# activity on the instances to scale up or down.  We don't
# show that here and since growing the cluster is a heavyweight
# operation especially for large databases it may be best
# to have a human control the cluster size using the
# desired_capacity value.
resource "aws_autoscaling_group" "ag-elb" {
  name = "ag-elb"
  launch_configuration = aws_launch_configuration.ag-elb.name
  max_size = 4
  min_size = 0
  desired_capacity = 2
  vpc_zone_identifier = [aws_subnet.ag-elb-a.id, aws_subnet.ag-elb-b.id, aws_subnet.ag-elb-c.id ]

  lifecycle {
     create_before_destroy = true
   }

}

# we link the target group with the autoscaling group so that the
# targets are whatever instances the autoscaling group has created
resource "aws_autoscaling_attachment" "ag-elb" {
   autoscaling_group_name = aws_autoscaling_group.ag-elb.id
   alb_target_group_arn = aws_lb_target_group.ag-elb.arn
}   


# After all is setup all one need to know to access the cluster
# is the address of the load balancer.  This will cause
# that address be printed when you do a 'terraform apply'
#
output "load-balancer-address" {
   value = aws_lb.ag-elb.dns_name
}   
