#!/bin/bash

declare -A hostaddr
hostaddr["prod_001"]="2001"
hostaddr["prod_002"]="2005"
hostaddr["prod_003"]="2004"
hostaddr["prod_004"]="2015"
hostaddr["dev_001"]="2008"
["demo_001"]="2008"
ssh -i ~/.ssh/NPC05027_cloud.pem kobayashi@sc_bastion -L 54322:production-db-stack-002-clouddbcluster-1wymkng9ugivu.cluster-ro-cyie0iqxl6ss.ap-northeast-1.rds.amazonaws.com:5432 -f -N
