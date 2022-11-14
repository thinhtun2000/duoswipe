/*
------------------------------------------------------------------------------
--                              Ada Web Server                              --
--                                                                          --
--                     Copyright (C) 2012-2020, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

pragma Style_Checks ("M32766");
--  Allow long lines

*/
/*
--  This package provides target dependent definitions of constant/types for
--  use by the AWS library. This package should not be directly with'd
--  by an application program.

--  This file is generated automatically, do not modify it by hand! Instead,
--  make changes to aws-os_lib-tmplt.c and rebuild the AWS library.
*/




/*

package AWS.OS_Lib is

   use Interfaces;
*/
/*

   ---------------------------------
   -- General platform parameters --
   ---------------------------------

   type OS_Type is (Windows, VMS, Other_OS);
*/
#define Target_OS            Windows
/*
   pragma Warnings (Off, Target_OS);
   --  Suppress warnings on Target_OS since it is in general tested for
   --  equality with a constant value to implement conditional compilation,
   --  which normally generates a constant condition warning.

*/
#define Target_Name          "x86_64-pc-mingw32"
/*

   Executable_Extension : constant String    := ".exe";
   Directory_Separator  : constant Character := '\';
   Path_Separator       : constant Character := ';';
*/
/*

   --  Sizes of various data types

*/
#define SIZEOF_unsigned_int  4
#define SIZEOF_fd_set        520
#define FD_SETSIZE           64
#define SIZEOF_sin_family    16
#define SIZEOF_nfds_t        32
#define SIZEOF_pollfd_events 16
#define SIZEOF_fd_type       64
#define SIZEOF_socklen_t     64
/*

   --  Sizes (in bytes) of the components of struct timeval
*/
#define SIZEOF_tv_sec        32
#define SIZEOF_tv_usec       32
/*

   --  Poll values

*/
#define POLLIN               1
#define POLLPRI              2
#define POLLOUT              4
#define POLLERR              8
#define POLLHUP              16
#define POLLNVAL             32
/*

   -----------------
   -- Fcntl flags --
   -----------------

*/
#define FNDELAY              -1
/*

   ----------------------
   -- Ioctl operations --
   ----------------------

*/
#define FIONBIO              -2147195266
#define FIONREAD             1074030207
#define FIONWRITE            -1
#define FIONSPACE            -1
/*

   ---------------------------------------
   -- getaddrinfo getnameinfo constants --
   ---------------------------------------

*/
#define AI_PASSIVE           1
#define AI_CANONNAME         2
#define AI_NUMERICSERV       -1
#define AI_NUMERICHOST       4
#define EAI_SYSTEM           11002
#define NI_NUMERICHOST       2
/*

   ------------------
   -- Errno values --
   ------------------

   --  The following constants are defined from <errno.h>

*/
#define EAGAIN               11
#define ENOENT               2
#define ENOMEM               12
/*

   --  The following constants are defined from <winsock2.h> (WSA*)

*/
#define EADDRNOTAVAIL        10049
#define EINPROGRESS          10036
#define EINTR                4
#define EINVAL               22
#define ENAMETOOLONG         38
#define ENOBUFS              10055
#define ENOTCONN             10057
#define ESHUTDOWN            10058
#define ESOCKTNOSUPPORT      10044
#define ETIMEDOUT            10060
#define ETOOMANYREFS         10059
#define EWOULDBLOCK          10035
#define ECONNRESET           10054
#define EACCES               13
#define EADDRINUSE           10048
#define EAFNOSUPPORT         10047
#define EALREADY             10037
#define EBADF                9
#define ECONNABORTED         10053
#define ECONNREFUSED         10061
#define EDESTADDRREQ         10039
#define EFAULT               14
#define EHOSTDOWN            10064
#define EHOSTUNREACH         10065
#define EIO                  5
#define EISCONN              10056
#define ELOOP                10062
#define EMFILE               24
#define EMSGSIZE             10040
#define EPIPE                32
#define EPFNOSUPPORT         10046
#define EPROTONOSUPPORT      10043
#define EPROTOTYPE           10041
#define ENETDOWN             10050
#define ENETRESET            10052
#define ENETUNREACH          10051
#define ENOPROTOOPT          10042
#define ENOTSOCK             10038
#define EOPNOTSUPP           10045
/*

   --------------
   -- Families --
   --------------

*/
#define AF_INET              2
#define AF_INET6             23
#define AF_UNSPEC            0
/*

   ------------------
   -- Socket modes --
   ------------------

*/
#define SOCK_STREAM          1
#define SOCK_DGRAM           2
/*

   -----------------
   -- Host errors --
   -----------------

*/
#define HOST_NOT_FOUND       11001
#define TRY_AGAIN            11002
#define NO_DATA              11004
#define NO_RECOVERY          11003
/*

   --------------------
   -- Shutdown modes --
   --------------------

*/
#define SHUT_RD              0
#define SHUT_WR              1
#define SHUT_RDWR            2
/*

   ---------------------
   -- Protocol levels --
   ---------------------

*/
#define SOL_SOCKET           65535
#define IPPROTO_IP           0
#define IPPROTO_IPV6         41
#define IPPROTO_UDP          17
#define IPPROTO_TCP          6
/*

   -------------------
   -- Request flags --
   -------------------

*/
#define MSG_OOB              1
#define MSG_PEEK             2
#define MSG_EOR              -1
#define MSG_WAITALL          8
#define MSG_NOSIGNAL         -1
#define MSG_Forced_Flags     0
/*
   --  Flags set on all send(2) calls
*/
/*

   --------------------
   -- Socket options --
   --------------------

*/
#define TCP_NODELAY          1
#define SO_REUSEADDR         4
#define SO_KEEPALIVE         8
#define SO_LINGER            128
#define SO_BROADCAST         32
#define SO_SNDBUF            4097
#define SO_RCVBUF            4098
#define SO_SNDTIMEO          4101
#define SO_RCVTIMEO          4102
#define SO_ERROR             4103
#define IP_MULTICAST_IF      9
#define IP_MULTICAST_TTL     10
#define IP_MULTICAST_LOOP    11
#define IP_ADD_MEMBERSHIP    12
#define IP_DROP_MEMBERSHIP   13
#define IP_PKTINFO           19
#define IPV6_V6ONLY          27
/*

   --  Some types

   type nfds_t is mod 2 ** SIZEOF_nfds_t;
   for nfds_t'Size use SIZEOF_nfds_t;

   type FD_Type
     is range -(2 ** (SIZEOF_fd_type - 1)) .. 2 ** (SIZEOF_fd_type - 1) - 1;
   for FD_Type'Size use SIZEOF_fd_type;

   type Events_Type is mod 2 ** SIZEOF_pollfd_events;
   for Events_Type'Size use SIZEOF_pollfd_events;

   type socklen_t is mod 2 ** SIZEOF_socklen_t;
   for socklen_t'Size use SIZEOF_socklen_t;

   type timeval_tv_sec_t
     is range -(2 ** (SIZEOF_tv_sec - 1)) .. 2 ** (SIZEOF_tv_sec - 1) - 1;
   for timeval_tv_sec_t'Size use SIZEOF_tv_sec;

   type timeval_tv_usec_t
     is range -(2 ** (SIZEOF_tv_usec - 1)) .. 2 ** (SIZEOF_tv_usec - 1) - 1;
   for timeval_tv_usec_t'Size use SIZEOF_tv_usec;

   type Timeval is record
      tv_sec  : timeval_tv_sec_t;  -- Seconds
      tv_usec : timeval_tv_usec_t; -- Microseconds
   end record;
   pragma Convention (C, Timeval);

   type sa_family_t is mod 2 ** SIZEOF_sin_family;
   for sa_family_t'Size use SIZEOF_sin_family;

   type In6_Addr is array (1 .. 8) of Interfaces.Unsigned_16;
   pragma Convention (C, In6_Addr);

   type Sockaddr_In6 is record
      Family   : sa_family_t := 0;
      Port     : Interfaces.C.unsigned_short := 0;
      FlowInfo : Interfaces.Unsigned_32 := 0;
      Addr     : In6_Addr := (others => 0);
      Scope_Id : Interfaces.Unsigned_32 := 0;
   end record;
   pragma Convention (C, Sockaddr_In6);

   type Addr_Info;
   type Addr_Info_Access is access all Addr_Info;

   type Addr_Info is record
      ai_flags     : C.int;
      ai_family    : C.int;
      ai_socktype  : C.int;
      ai_protocol  : C.int;
      ai_addrlen   : socklen_t;
      ai_canonname : C.Strings.chars_ptr;
      ai_addr      : System.Address;
      ai_next      : Addr_Info_Access;
   end record;
   pragma Convention (C, Addr_Info);
*/
/*

   --  Some routines

   function GetAddrInfo
     (node    : C.Strings.chars_ptr;
      service : C.Strings.chars_ptr;
      hints   : Addr_Info;
      res     : not null access Addr_Info_Access) return C.int;

   procedure FreeAddrInfo (res : Addr_Info_Access);

   function GAI_StrError (ecode : C.int) return C.Strings.chars_ptr;

   function Socket_StrError (ecode : Integer) return C.Strings.chars_ptr;

   function Set_Sock_Opt
     (S       : C.int;
      Level   : C.int;
      OptName : C.int;
      OptVal  : System.Address;
      OptLen  : C.int) return C.int;

   function C_Ioctl (S : C.int; Req : C.int; Arg : access C.int) return C.int;

   function C_Close (Fd : C.int) return C.int;
*/
/*

   pragma Linker_Options ("-lws2_32");

   procedure WSA_Startup (Version : C.int; Data : System.Address);
   pragma Import (Stdcall, WSA_Startup, "WSAStartup");

   function Socket_Errno return Integer;
*/
/*

   ------------------------------
   -- MinGW-specific constants --
   ------------------------------

*/
#define WSASYSNOTREADY       10091
#define WSAVERNOTSUPPORTED   10092
#define WSANOTINITIALISED    10093
#define WSAEDISCON           10101
/*

private

*/
/*
   pragma Import (Stdcall, GetAddrInfo, "getaddrinfo");
   pragma Import (Stdcall, FreeAddrInfo, "freeaddrinfo");
   pragma Import (Stdcall, Set_Sock_Opt, "setsockopt");

   pragma Import (C, GAI_StrError, "AWS_gai_strerror");
   pragma Import (C, Socket_StrError, "socket_strerror");
   pragma Import (Stdcall, C_Ioctl, "ioctlsocket");
   pragma Import (Stdcall, C_Close, "closesocket");
   pragma Import (Stdcall, Socket_Errno, "WSAGetLastError");
*/
/*

end AWS.OS_Lib;
*/
