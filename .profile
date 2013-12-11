######################################################################
# Put this in your remote system's .profile for remote bash to track
# your current dir
set_eterm_dir () {
    echo -e "\033AnSiTu" "$LOGNAME" # $LOGNAME is more portable than using whoami.
    echo -e "\033AnSiTc" "$(pwd)"
    if [ $(uname) = "SunOS" ]; then
	    # The -f option does something else on SunOS and is not needed anyway.
       	hostname_options="";
    else
        hostname_options="-f";
    fi
    echo -e "\033AnSiTh" "$(hostname $hostname_options)" # Using the -f option can cause problems on some OSes.
    history -a # Write history to disk.
}

# Track directory, username, and cwd for remote logons.
if [ "$TERM" = "eterm-color" ]; then
    PROMPT_COMMAND=set_eterm_dir
fi
######################################################################
