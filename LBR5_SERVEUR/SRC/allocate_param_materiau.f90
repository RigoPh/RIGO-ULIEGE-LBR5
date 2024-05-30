subroutine allocate_param_materiau()

use param_section

implicit double precision (a-h,o-z)

allocate (e(nbrMat))
allocate (eta(nbrMat))
allocate (sigy(nbrMat))
allocate (sigyadm(nbrMat))
allocate (coefk(nbrMat))		!r&d15
allocate (spec(nbrMat))

allocate (dref(nbrMat))	!jan09
allocate (drefx(nbrMat)) !jan09
allocate (drefy(nbrMat)) !jan09
allocate (dref_b(nbrMat))!jan09
allocate (dref_c(nbrMat))!jan09
allocate (dref_l(nbrMat))!jan09
allocate (c1(nbrMat))	!jan09
allocate (c2(nbrMat))	!jan09
allocate (c3(nbrMat))	!jan09
allocate (cout(nbrMat))	!jan09
allocate (c_pb(nbrMat))	!jan09
allocate (c_tc(nbrMat))  !jan09
allocate (c_tl(nbrMat))  !jan09
allocate (dc1(nbrMat))	!jan09
allocate (dw2(nbrMat))	!jan09
allocate (dw3(nbrMat))	!jan09
allocate (dw_b(nbrMat))	!jan09
allocate (dw_c(nbrMat))  !jan09
allocate (dw_l(nbrMat))  !jan09
allocate (labour_cost(nbrMat))  !jan09
allocate (p10(nbrMat))   !jan09
allocate (dp10(nbrMat))  !jan09
allocate (p4(nbrMat))    !jan09
allocate (dp4(nbrMat))   !jan09
allocate (p5(nbrMat))    !jan09
allocate (dp5(nbrMat))   !jan09
allocate (p9x(nbrMat))   !jan09
allocate (p9y(nbrMat))   !jan09
allocate (dp9x(nbrMat))  !jan09
allocate (dp9y(nbrMat))  !jan09
allocate (ber(nbrMat))   !jan09
allocate (bet(nbrMat))   !jan09
allocate (p6(nbrMat))    !jan09
allocate (p7(nbrMat))    !jan09
allocate (c8(nbrMat))    !jan09
allocate (dc8(nbrMat))   !jan09
allocate (ialr(nbrMat))   !jan09
allocate (ialt(nbrMat))   !jan09

return
end
