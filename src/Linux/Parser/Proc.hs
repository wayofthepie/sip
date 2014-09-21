module Linux.Parser.Proc where

import Data.ByteString

-- | Data type for \/proc\/[pid]\/stat. See __man_proc__ for more in 
-- depth information.
data ProcessStat = PS {
        _pid        :: ByteString, -- ^ PID 
        _comm       :: ByteString, -- ^ File name of executable
        _state      :: ByteString, -- ^ Processes state 
        _ppid       :: ByteString, -- ^ Parent processes PID
        _pgrp       :: ByteString, -- ^ Process group ID
        _session    :: ByteString, -- ^ Session ID 
        _tty_nr      :: ByteString, -- ^ Controlling terminal

         -- | Foreground process group ID of controlling terminal
        _tpgid      :: ByteString,
        _flags      :: ByteString, -- ^ Kernel flags word

        -- | The number of minor faults the process has made which have not 
        -- required loading a memory page from disk.
        _minflt     :: ByteString, 

        -- | The number of minor faults that the process's waited-for children 
        -- have made.
        _cminflt    :: ByteString,
        
        -- | The number of major faults the process has made which have 
        -- required loading a memory page from disk.
        _majflt     :: ByteString, 

        -- |The number of major faults that the process's waited-for children 
        -- have made.
        _cmajflt    :: ByteString,
        
        -- | Amount of time, in clock ticks, scheduled in user mode.
        _utime      :: ByteString, 

        -- | Amount of time, in clock ticks, that this process's waited-for 
        -- children have been scheduled in user  mode.
        _stime      :: ByteString, 
        
        -- | Amount of time that this process's waited-for children have 
        -- been scheduled in user  mode,  measured  in  clock  ticks
        -- (divide  by sysconf(_SC_CLK_TCK)).
        _cutime     :: ByteString,


        -- | Amount  of  time  that  this  process's waited-for children 
        -- have been scheduled in kernel mode, measured in clock ticks 
        -- (divide by sysconf(_SC_CLK_TCK)).
        _cstime     :: ByteString,

        _priority   :: ByteString, -- ^ Processes prority
        _nice       :: ByteString, -- ^ Processes nice value
        _num_threads:: ByteString, -- ^ Number of threads in this process

        -- | The  time  in jiffies before the next SIGALRM is sent to the 
        -- process due to an interval timer.
        _itrealvalue:: ByteString,

        -- | Time, in clock ticks, the process started after system boot.
        _starttime  :: ByteString,

        _vsize      :: ByteString, -- ^ Virtual memory size in bytes.
        _rss        :: ByteString, -- ^ Resident set size.
        
        -- | Current soft limit in bytes on the rss of the process.
        _rsslim     :: ByteString,

        -- | Address above which program text can run.
        _startcode  :: ByteString,

        -- | Address below which program text can run.
        _endcode    :: ByteString,


        _startstack :: ByteString, -- ^ Address of the start of the stack.
        _kstkesp    :: ByteString, -- ^ Current value of ESP (stack pointer).
        _kstkeip    :: ByteString, -- ^ Current EIP (instruction pointer).
        
        -- | The bitmap of pending signals, displayed as a decimal number. 
        -- Obsolete, use __\/proc\/[pid]\/status__ instead.
        _signal     :: ByteString, 

        -- |  The bitmap of blocked signals, displayed as a decimal number. 
        -- Obsolete, use __\/proc\/[pid]\/status__ instead.
        _blocked    :: ByteString, 
        
        -- | The bitmap of ignored signals, displayed as a decimal number. 
        -- Obsolete, use __\/proc\/[pid]\/status__ instead.
        _sigignore  :: ByteString, 

        -- | The  bitmap of caught signals, displayed as a decimal number.
        -- Obsolete, use __\/proc\/[pid]\/status__ instead.
        _sigcatch   :: ByteString, 

        _wchan      :: ByteString, -- ^ Channel which the process is waiting.

        -- | Number of pages swapped (not maintained).     
        _nswap      :: ByteString, 
        
        -- | Cumulative __nswap__ for child processes (not maintained).
        _cnswap     :: ByteString, 
        
        -- | Signal to be sent to parent when process dies.
        _exiti_signal :: ByteString, 

        _processor  :: ByteString, -- ^ CPU number last executed on.
        _rt_priority:: ByteString, -- ^ Real-time scheduling priority.
        _policy     :: ByteString, -- ^ Scheduling policy.

        -- | Aggregated block IO delays.
        _delayacct_blkio_ticks :: ByteString, 
        
        -- |  Guest time of the process (time spent running a virtual CPU for 
        -- a guest operating system), measured  in  clock  ticks  (divide  by
        -- sysconf(_SC_CLK_TCK)).
        _guest_time :: ByteString, 
        
        -- | Guest time of the process's children, measured in clock ticks 
        -- (divide by sysconf(_SC_CLK_TCK)).
        _cguest_time:: ByteString, 

        -- | Address above which program initialized and uninitialized (BSS) 
        -- data are placed.
        _start_data :: ByteString, 
        
        -- | Address below which program initialized and uninitialized (BSS) 
        -- data are placed.
        _end_data   :: ByteString, 

        -- | Address above which program heap can be expanded with brk(2).
        _start_brk  :: ByteString,


        -- | Address above which program command-line arguments (argv) are placed.
        _arg_start  :: ByteString,

        -- | Address below program command-line arguments (argv) are placed.
        _arg_end    :: ByteString,

        -- | Address above which program environment is placed.
        _env_start  :: ByteString,
    
        -- | Address below which program environment is placed
        _env_end    :: ByteString,

        -- | The thread's exit status in the form reported by waitpid(2).
        _exit_code  :: ByteString
    } deriving (Eq, Show)


