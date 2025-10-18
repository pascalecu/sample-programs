module utils
   implicit none
contains
   function get_arg(index) result(arg)
      integer, intent(in) :: index
      character(len=:), allocatable :: arg
      integer :: n, err

      call get_command_argument(index, length=n, status=err)
      if (err /= 0 .or. n <= 0) then
         arg = ''
         return
      end if

      allocate(character(len=n) :: arg)
      call get_command_argument(index, arg, status=err)
      if (err /= 0) arg = trim(adjustl(arg))
   end function get_arg

   function str2int(s) result(val)
      character(len=*), intent(in) :: s
      character(len=:), allocatable :: tmp
      integer :: val
      tmp = trim(adjustl(s))
      read(tmp, *) val
   end function str2int

   function str2list(s) result(arr)
      character(len=*), intent(in) :: s
      integer, allocatable :: arr(:)
      integer :: commas, i, j, idx, start

      commas = count([ (s(j:j) == ',', j = 1,len_trim(s)) ])
      allocate(arr(commas+1))

      start = 1
      idx = 1
      do i = 1, len_trim(s)+1
         if (i > len_trim(s) .or. s(i:i) == ',') then
            arr(idx) = str2int(s(start:i-1))
            idx = idx + 1
            start = i + 1
         end if
      end do
   end function str2list

   pure function bool2str(flag) result(s)
      logical, intent(in) :: flag
      character(len=:), allocatable :: s

      if (flag) then
         s = 'true'
      else
         s = 'false'
      end if
   end function bool2str
end module utils

program binary_search
   use utils
   implicit none

   character(len=:), allocatable :: list_arg, target_arg
   integer, allocatable :: haystack(:)
   integer :: needle
   logical :: found

   if (command_argument_count() /= 2) call usage()

   list_arg   = get_arg(1)
   target_arg = get_arg(2)

   if (len_trim(list_arg) == 0 .or. len_trim(target_arg) == 0) call usage()

   haystack = str2list(list_arg)
   needle   = str2int (target_arg)

   if (.not. is_sorted(haystack)) call usage()

   found = bsearch(haystack, needle)

   print *, bool2str(found)

contains
   subroutine usage()
      print '(A)', 'Usage: please provide a list of sorted integers ("1, 4, 5, 11, 12") and the integer to find ("11")'
      stop
   end subroutine usage

   pure logical function is_sorted(arr)
      integer, intent(in) :: arr(:)
      is_sorted = size(arr) <= 1 .or. all(arr(2:) >= arr(1:size(arr)-1))
   end function is_sorted

   pure logical function bsearch(lst, value)
      integer, intent(in) :: lst(:), value
      integer :: left, right, mid
      integer :: first, last
      integer :: len

      len = size(lst)
      if (len == 0) then
         bsearch = .false.
         return
      end if

      if (len == 1) then
         bsearch = (lst(1) == value)
         return
      end if

      first = lst(1)
      last  = lst(len)
      if (value < first .or. value > last) then
         bsearch = .false.
         return
      end if

      left = 1
      right = len
      do while (left <= right)
         mid = left + (right - left)/2
         if (lst(mid) == value) then
            bsearch = .true.
            return
         elseif (lst(mid) < value) then
            left = mid + 1
         else
            right = mid - 1
         end if
      end do

      bsearch = .false.
   end function bsearch
end program binary_search
