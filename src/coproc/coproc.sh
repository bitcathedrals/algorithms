#
# coproc-em
#

function coproc_is_named {          # the pipline string

  # returns true when the IPC mechanism is named, false when a anonymous
  # pipe is requested

  expr >/dev/null 2>&1 "$1" : ".*<IPC>.*" && return 0
  return 1;
}

function coproc_open {              # Name of the program.
  local ipc_cmd="$1"; local batch="$1"; shift;

                                    # Arguements for the program in a
                                    # single quoted string. The
                                    # arguements should configure the
                                    # program for stream I/O, taking
                                    # generated input on STDIN and
                                    # output on STDOUT. When the
                                    # generated input to the
                                    # co-process must be specified as
                                    # a path to an arguement use the
                                    # marker "<IPC>" where the value
                                    # goes, and a IPC mechanism will
                                    # be substituted.  
                           
                                    # A post co-process pipeline can
                                    # also be placed in the string.

  local pipeline="$1"; shift;

                                    # A optional flag for specifying
                                    # the co-process (input) write
                                    # mechanism. The default is
                                    # tmpfile for compatibility. pipe
                                    # can be specified. The "-dc" dump
                                    # code program option will force
                                    # a tmpfile and terminate the script
                                    # when generated output to the
                                    # co-process is completed.

  #
  # allocate a coproc_handle
  #

  fd_handle_new "$ipc_cmd" 
  fd_push_handle $?

  #
  # setup debugging when requested
  #

  if [[ "$arg_dbg" = "enabled" ]] ; then
    # when debugging is enabled, duplicate the output of the coprocess
    # to a file named after the program in the TMPDIR area.

    batch="$batch 2>${fd_tmp}.coproc_stderr"

    : >|"${fd_tmp}.coproc_r";
    pipeline="$pipeline | tee -a \"${fd_tmp}.coproc_stdout\""

    print -u2 "coproc-em: coproc_open debug - writing stderr, and stdout \
logs with the extensions \".coproc_stderr\" and \".coproc_stdout\"";
  fi

  if [[ "$arg_dump" = "enabled" ]] ; then
    print -u2 "tool dump: writing IPC data to file \"${fd_tmp}.coproc_stdin\"";

    # override user selected IPC mechanisms with a tmpfile when
    # dumping code to save the stream written to the co-process

    1=tmp
  fi

  #
  # select the best IPC write mechanism possible. This is done first
  # because the read mechanism depends entirely on the write mechanism
  # available.
  #

  # default to a tmp file for now due to the horrid state of affairs
  # in regards to streaming IO.

  (( ${+1} )) || 1=tmp;

  case "$1" in
  tmp) fd_req_wd 1;;
  pipe)
    # handle the pipe case, with fallback on a named pipe, either
    # method increment the streaming level

    if coproc_is_named "$pipeline" ; then
      [[ -p "${fd_tmp}.coproc_stdin" ]] || mkfifo "${fd_tmp}.coproc_stdin"
      fd_req_wd 2
    else
      fd_req_wd 3 # builtin-coprocess
    fi
  ;;
  *)
    print -u2 "coproc_open: unkown IPC mechanism \"$1\", aborting open"
    return ${external_ERR:-1}
  ;;
  esac

  # named IPC methods need to substitute a path for the IPC marker.

  coproc_is_named "$pipeline" && read pipeline \
< <(echo "$pipeline" | sed -e "s,<IPC>,${fd_tmp}.coproc_stdin,");

  #
  # delay the actuall openning of the writer, finish the reader
  # first, as named pipes require the reader to open first.
  #

  case $fd_w_lvl in
  1)
    # for tmp files delay until the all input has been written to the
    # tmp file. Stash the pipeline in the tmp path feild for use by
    # coproc_flush. Open the writer and return to construct the input
    # for the co-process.

    eval exec "$fd_wd>|\"${fd_tmp}.coproc_stdin\""
    fd_using_wd
    
    fd_set_tmp "<($batch $pipeline&)"
  ;;
  2)
    # pipes on both ends of some sort, use the process redirect.
    # open first the reader, then the writer.

    fd_req_rd 2
    eval "exec $fd_rd< <($batch $pipeline&)"
    fd_using_rd

    eval exec "$fd_wd>|\"${fd_tmp}.coproc_stdin\""
    fd_using_wd
  ;;
  3)
    # use the Zsh builtin coproc implementation.  both read/write
    # descriptors are in use immediately, so update the handle state
    # and return.

    fd_req_rd 3
    eval "coproc $batch $pipeline"

    fd_using_rd
    fd_using_wd
  ;;
  esac

  return 0;
}

function coproc_flush {
  (( ${+1} )) || {
    [[ -o interactive ]] && \
print -u2 "coproc-em: coproc_flush requires a fd_alloc handle as an arguement"
    return ${external_ERR:-1}
  }

  fd_push_handle $1

  case $fd_w_lvl in
  1)
    fd_close_wd
    [[ "$arg_dump" = "enabled" ]] && exit ${no_ERR:-0};

    fd_req_rd 2
    eval "exec $fd_rd< $fd_tmp"
    fd_using_rd
  ;;
  2) fd_close_wd ;;
  3) ;;
  esac

  fd_pop_handle
  return 0;
}

function coproc_write (

  # write to the co-process. Re-direct to the function or from
  # blocks. 

  ERRNO=0; # function as a sub-shell to de-conflict the ERRNO value

  function TRAPDEBUG {
    (( ERRNO == POSIX_EINTR )) && {ERRNO="0";};

    if (( ERRNO != 0 )) ; then
      print -u2 "tool error: system error code \"$ERRNO\" received in coproc_write, data value is \"$data\""
      coproc_dump "coproc_write TRAPDEBUG"
      exit $external_ERR;
    fi;
  }

  local data;
  while read data; do 
    print - "$data"; 
  done >&$fd_wd;
)

function coproc_close {   # takes an optional handle arguement
  (( ${+1} )) || {
    [[ -o interactive ]] && \
print -u2 "coproc-em: coproc_close requires a fd_alloc handle as an arguement"
    return ${external_ERR:-1}
  }

  fd_handle_free $1
  return 0;
}

function coproc_em {
  return 0;
}