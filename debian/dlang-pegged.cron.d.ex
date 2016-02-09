#
# Regular cron jobs for the dlang-pegged package
#
0 4	* * *	root	[ -x /usr/bin/dlang-pegged_maintenance ] && /usr/bin/dlang-pegged_maintenance
