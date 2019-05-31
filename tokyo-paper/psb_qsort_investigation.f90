! ====== ORIGINAL ======
submodule (psb_sort_mod) psb_qsort_impl_mod
contains

subroutine psb_@X@qsort(x,ix,dir,flag)
  ! use psb_sort_mod, psb_protect_name => psb_@X@qsort   ! will just be submodule
  use psb_error_mod
  implicit none
  @TYPE@(@FKIND@), intent(inout)  :: x(:)
  integer(psb_ipk_), optional, intent(in)    :: dir, flag
  integer(@IXKIND@), optional, intent(inout) :: ix(:)

  integer(psb_ipk_) :: dir_, flag_, err_act, i
  integer(@IXKIND@) :: n
  integer(psb_ipk_)  :: ierr(5)
  character(len=20)  :: name
end subroutine psb_@X@qsort(x,ix,dir,flag)
! ===== END ORIGINAL =====

! =====================
!    ==TEMPLATE==
! =====================
TEMPLATE SUBROUTINE psb_qsort(x,ix,dir,flag)   ! ==TEMPLATE==
  use psb_sort_mod, psb_protect_name => psb_qsort
  use psb_error_mod
  implicit none
  NUMERIC(*), intent(inout)  :: x(:)              ! ==TEMPLATE==
  integer(psb_ipk_), optional, intent(in)    :: dir, flag
  INTEGER(*), optional, intent(inout) :: ix(:) ! ==TEMPLATE==

  integer(psb_ipk_) :: dir_, flag_, err_act, i
  INTEGER(*) :: n                              ! ==TEMPLATE==
  integer(psb_ipk_)  :: ierr(5)
  character(len=20)  :: name
END TEMPLATE SUBROUTINE psb_qsort
! =====================
!   ==END TEMPLATE==
! =====================

! =====================
!      ==MACRO==
! =====================
DEFINE MACRO generic_psb_qsort(array_of_types, array_of_kinds)
  MACRO INTEGER  :: i, j, kind
  MACRO NUMERIC  :: typename          ! THIS IS MIXING WITH TEMPLATES
  MACRO DO i=1, SIZE(array_of_types)
    MACRO DO j=1, SIZE(array_of_kinds)
      MACRO typename=array_of_types(i)
      MACRO kind=array_of_types(i)

subroutine psb_%%typename%%qsort(x,ix,dir,flag)
  use psb_error_mod
  implicit none
  typename(kind), intent(inout)  :: x(:)
  integer(psb_ipk_), optional, intent(in)    :: dir, flag
  integer(kind), optional, intent(inout) :: ix(:)

  integer(psb_ipk_) :: dir_, flag_, err_act, i
  integer(kind) :: n
  integer(psb_ipk_)  :: ierr(5)
  character(len=20)  :: name
end subroutine psb_%%typename%%qsort(x,ix,dir,flag)
    END MACRO DO
  END MACRO DO
END MACRO generic_psb_qsort

INTEGER,  PARAMETER :: kinds(2)=[4,8,16]
NUMERIC, PARAMETER :: types(3)=[integer,float,double]
EXPAND generic_psb_qsort(types,kinds)

! OR ANOTHER WAY, MORE CORRECT BECAUSE ARRAY OF TYPES DOESN'T EXIST
INTEGER,PARAMETER :: kinds(2)=[4,8,16]
EXPAND generic_psb_qsort(integer,kinds)
EXPAND generic_psb_qsort(float,kinds)
EXPAND generic_psb_qsort(double,kinds)
! =====================
!    ==END MACRO==
! =====================



! ====== ORIGINAL CASES ======
select case(dir_)
  @REALS@
case (psb_sort_up_)
  call psi_@X@qsrx_up(n,x,ix)
case (psb_sort_down_)
  call psi_@X@qsrx_dw(n,x,ix)
  @REALE@
  @CPLXS@
case (psb_lsort_up_)
  call psi_@X@lqsrx_up(n,x,ix)
case (psb_lsort_down_)
  call psi_@X@lqsrx_dw(n,x,ix)
case (psb_alsort_up_)
  call psi_@X@alqsrx_up(n,x,ix)
case (psb_alsort_down_)
  call psi_@X@alqsrx_dw(n,x,ix)
  @CPLXE@
case (psb_asort_up_)
  call psi_@X@aqsrx_up(n,x,ix)
case (psb_asort_down_)
  call psi_@X@aqsrx_dw(n,x,ix)
case default
  ierr(1) = 3; ierr(2) = dir_;
  call psb_errpush(psb_err_input_value_invalid_i_,name,i_err=ierr)
  goto 9999
end select
! =====================
!    ==TEMPLATE==
! =====================

if (type(x) .eq. REAL) then
  select case(dir_)
    case (psb_sort_up_)
      call psi_@X@qsrx_up(n,x,ix)
    case (psb_sort_down_)
      call psi_@X@qsrx_dw(n,x,ix)
  case (psb_asort_up_)
    call psi_@X@aqsrx_up(n,x,ix)
  case (psb_asort_down_)
    call psi_@X@aqsrx_dw(n,x,ix)
  case default
    ierr(1) = 3; ierr(2) = dir_;
    call psb_errpush(psb_err_input_value_invalid_i_,name,i_err=ierr)
    goto 9999
  end select
end if
if (type(x) .eq. COMPLEX) then
  select case(dir_)
    case (psb_lsort_up_)
      call psi_@X@lqsrx_up(n,x,ix)
    case (psb_lsort_down_)
      call psi_@X@lqsrx_dw(n,x,ix)
    case (psb_alsort_up_)
      call psi_@X@alqsrx_up(n,x,ix)
    case (psb_alsort_down_)
      call psi_@X@alqsrx_dw(n,x,ix)
  case (psb_asort_up_)
    call psi_@X@aqsrx_up(n,x,ix)
  case (psb_asort_down_)
    call psi_@X@aqsrx_dw(n,x,ix)
  case default
    ierr(1) = 3; ierr(2) = dir_;
    call psb_errpush(psb_err_input_value_invalid_i_,name,i_err=ierr)
    goto 9999
  end select
end if

! =====================
!   ==END TEMPLATE==
! =====================
! =====================
!      ==MACRO==
! =====================
DEFINE MACRO generic_psb_qsort_case(typename,X)
  select case(dir_)
    MACRO IF (typename .eq. REAL)
      case (psb_sort_up_)
        call psi_%%X%%qsrx_up(n,x,ix)
      case (psb_sort_down_)
        call psi_%%X%%qsrx_dw(n,x,ix)
    END MACRO IF
    MACRO IF (typename .eq. COMPLEX)
      case (psb_lsort_up_)
        call psi_%%X%%lqsrx_up(n,x,ix)
      case (psb_lsort_down_)
        call psi_%%X%%lqsrx_dw(n,x,ix)
      case (psb_alsort_up_)
        call psi_%%X%%alqsrx_up(n,x,ix)
      case (psb_alsort_down_)
        call psi_%%X%%alqsrx_dw(n,x,ix)
    END MACRO IF
    case (psb_asort_up_)
      call psi_%%X%%aqsrx_up(n,x,ix)
    case (psb_asort_down_)
      call psi_%%X%%aqsrx_dw(n,x,ix)
    case default
      ierr(1) = 3; ierr(2) = dir_;
      call psb_errpush(psb_err_input_value_invalid_i_,name,i_err=ierr)
      goto 9999
    end select
 END MACRO generic_psb_qsort_case

 EXPAND generic_psb_qsort_case(type(x),'SOMETHING TO REPLACE X')

! =====================
!    ==END MACRO==
! =====================

! =====================
!  CONCLUSIONS
! =====================
 We should be able to mix and match macros and generics.
   Need to figure out best way to do this
 select case for templates really should use macros, default cases need to be repeated and without macros changes to one part of the code could be forgetten to be updated
 NEED: we need something like:
         NUMERIC type  :: convinient for programmer in macros, required for templates
           or
         GENERIC TYPE(T) => integer(4), integer(8), real(8)

Benefit: GENERIC TYPE(T) the compiler can limited the amound of code generated!
How would I define the sparse matrix data structures
