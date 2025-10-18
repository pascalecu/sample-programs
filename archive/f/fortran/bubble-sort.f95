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

   pure function int2str(val) result(s)
      integer, intent(in) :: val
      character(len=:), allocatable :: s
      character(len=64) :: buf
      write(buf, '(I0)') val
      s = trim(buf)
   end function int2str

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

   function list2str(arr) result(s)
      implicit none
      integer, intent(in) :: arr(:)
      character(len=:), allocatable :: s
      integer :: i, total_len

      if (size(arr) == 0) then
         s = ""
         return
      end if

      total_len = 0
      do i = 1, size(arr)
         total_len = total_len + len(int2str(arr(i)))
      end do
      total_len = total_len + 2*(size(arr)-1)  ! add ", " separators

      allocate(character(len=total_len) :: s)
      write(s, '( *(I0, :, ", ") )') arr
      s = trim(s)
   end function list2str

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

program bubblesort
   use utils
   implicit none

   character(len=:), allocatable :: list_arg
   integer, allocatable :: lst(:)

   if (command_argument_count() /= 1) call usage()

   list_arg = get_arg(1)
   if (len_trim(list_arg) == 0) call usage()

   lst = str2list(list_arg)
   if (size(lst) < 2) call usage()

   call bubble_sort(lst)

   print '(A)', list2str(lst)

contains
   subroutine usage()
      print '(A)', 'Usage: please provide a list of at least two integers to sort in the format "1, 2, 3, 4, 5"'
      stop
   end subroutine usage

   pure logical function is_sorted(arr)
      integer, intent(in) :: arr(:)
      is_sorted = size(arr) <= 1 .or. all(arr(2:) >= arr(1:size(arr)-1))
   end function is_sorted

   pure subroutine bubble_sort(arr)
      integer, intent(inout) :: arr(:)
      integer :: i, j, tmp
      logical :: swapped

      do i = size(arr) - 1, 1, -1
         swapped = .false.
         do j = 1, i
            if (arr(j) > arr(j + 1)) then
               tmp = arr(j)
               arr(j) = arr(j + 1)
               arr(j + 1) = tmp
               swapped = .true.
            end if
         end do
         if (.not. swapped) exit
      end do
   end subroutine bubble_sort

end program bubblesort
