thin-cluster
============

Hypothetical simple clustered volume manager built on top of Linux dm-thin.

Design notes
------------

An external cluster membership service will tell us when particular nodes have been fenced and nominate a cluster master from among the live nodes (like XenServer HA). Perhaps this service should run scripts when events are detected (this would make integration with power-fencing easier)

For everything else, rather than creating another monolithic 'service', let's see if we can make a set of command-line tools. After all, dm-thin provides all the performance-critical stuff, all we need to do is the occasional control-plane operation.

Each node will be assigned a slice of the disk by the cluster master for local allocation via dm-thin.
* when the node is running low it will request more space
* should we pre-allocate the whole disk and have a ballooning-like protocol to return the space?

The cluster master will control a metadata volume (in custom format?) for all offline user volumes and for remembering each host's private metadata volume. This is like LVM.

Each volume has a unique number, starting from 1 (and < 2 ** 24). This will be used to identify the disk to dm-thin.

There will be one 'global data volume' mapped onto every node where all data blocks are visible. Using one single 'global data volume' means the data block numbers are valid cluster-wide and don't need remapping or renumbering when blocks are moved out.

Every node will contain a private metadata volume tracking local allocations for online volumes. All areas of the 'global data volume' which are not assigned to this host by the cluster master will be associated to disk 0, which is never activated or read from/written to.

Operation: export: this will emit a stream of physical block addresses for a given user volume. This would be the base for a 'vhd export' function and a 'move volume to another pool'

Operation: detach: this will add all the blocks from the given user volume to disk 0, ie owned by the "cluster".

Operation: attach: this will remove the given blocks from disk 0, to a fresh disk and activates it.

