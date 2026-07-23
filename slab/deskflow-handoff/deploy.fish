#!/usr/bin/env fish
set here (cd (dirname (status filename)); and pwd)
set stage /tmp/deskflow-handoff
set neo_fp 76208504df49a431b6183cadb6478ba8bc43b1f14d7f12e4fa4772d48404834c
set blueberry_fp 0f3ad9948c903ef4881a3e7092ea511572b4d57bbe12abac2993bab7252d0ce1
set chicken_fp 381baab53079460f450bddf13243742c6694882367cad52f2410d5f92067918a
set panda_fp 268c2fff5eb113a66d7b13b51d858630b034b04f547579b41ec9b111882a5803
set blueberry_ssh jas@100.79.75.53
set chicken_ssh fusermacminichicken@100.98.158.126
set panda_ssh fusermacminipanda@100.88.155.94
set ssh_opts -o BatchMode=yes -o ConnectTimeout=8 -o ConnectionAttempts=1 -o ControlMaster=no -o ControlPath=none

for host in $blueberry_ssh $chicken_ssh $panda_ssh
    ssh $ssh_opts $host "rm -rf $stage; mkdir -p $stage"
    or exit 1
    scp -q $ssh_opts $here/deskflow-* $here/unipointer.swift $here/install.sh $host:$stage/
    or exit 1
end

bash $here/install.sh --defer-start --machine neo --screen-name neo --address 100.108.5.81 --controller --clients $blueberry_ssh,$chicken_ssh,$panda_ssh --role server --server-host 100.108.5.81 --trusted-servers $blueberry_fp --trusted-clients $blueberry_fp,$chicken_fp,$panda_fp
or exit 1
ssh $ssh_opts $blueberry_ssh "bash $stage/install.sh --defer-start --machine blueberry --screen-name blueberry.local --address 100.79.75.53 --controller --clients jas@100.108.5.81,fusermacminichicken@100.98.158.126,fusermacminipanda@100.88.155.94 --role client --server-host 100.108.5.81 --trusted-servers $neo_fp --trusted-clients $neo_fp,$chicken_fp,$panda_fp"
or exit 1
ssh $ssh_opts $chicken_ssh "bash $stage/install.sh --defer-start --machine chicken --screen-name chicken.local --address 100.98.158.126 --role client --server-host 100.108.5.81 --trusted-servers $neo_fp,$blueberry_fp"
or exit 1
ssh $ssh_opts $panda_ssh "bash $stage/install.sh --defer-start --machine panda --screen-name panda.local --address 100.88.155.94 --role client --server-host 100.108.5.81 --trusted-servers $neo_fp,$blueberry_fp"
or exit 1

~/.local/bin/deskflow-start
or exit 1
for host in $blueberry_ssh $chicken_ssh $panda_ssh
    ssh $ssh_opts $host '~/.local/bin/deskflow-start'
    or exit 1
end

echo "Deskflow handoff deployed; neo remains controller until blueberry's trackpad is touched."
