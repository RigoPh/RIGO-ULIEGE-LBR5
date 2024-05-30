subroutine put_param_materiau()


use param_section
use PARAM_SECTION_VECTOR
!use param_cout_vector
!use param_multiobj
!use param_multiobj_vector



e_vector    (1:nbrMat,iboat)    =  e    (1:nbrMat)
eta_vector  (1:nbrMat,iboat)    =  eta (1:nbrMat)
sigy_vector (1:nbrMat,iboat)    =  sigy (1:nbrMat)
sigyadm_vector (1:nbrMat,iboat) =  sigyadm (1:nbrMat)
coefk_vector(1:nbrMat,iboat)    =  coefk(1:nbrMat)			
spec_vector (1:nbrMat,iboat)    =  spec (1:nbrMat)

!rend_vector (1:nbrMat,iboat)=rend(1:nbrMat)
!eqp_vector (1:nbrMat,iboat)=eqp(1:nbrMat)
dref_vector (1:nbrMat,iboat)=dref(1:nbrMat)
drefx_vector(1:nbrMat,iboat)=drefx(1:nbrMat)
drefy_vector(1:nbrMat,iboat)=drefy(1:nbrMat)

dref_b_vector(1:nbrMat,iboat)=dref_b(1:nbrMat)
dref_l_vector(1:nbrMat,iboat)=dref_l(1:nbrMat)
dref_c_vector(1:nbrMat,iboat)=dref_c(1:nbrMat)

c1_vector (1:nbrMat,iboat)=c1(1:nbrMat)
dc1_vector(1:nbrMat,iboat)=dc1(1:nbrMat)
c8_vector (1:nbrMat,iboat)=c8(1:nbrMat)
dc8_vector(1:nbrMat,iboat)=dc8(1:nbrMat)

c2_vector  (1:nbrMat,iboat)=c2(1:nbrMat)
c3_vector  (1:nbrMat,iboat)=c3(1:nbrMat)
cout_vector(1:nbrMat,iboat)=cout(1:nbrMat)

c_pb_vector  (1:nbrMat,iboat)=c_pb(1:nbrMat)
c_tc_vector  (1:nbrMat,iboat)=c_tc(1:nbrMat)
c_tl_vector  (1:nbrMat,iboat)=c_tl(1:nbrMat)

dw2_vector  (1:nbrMat,iboat)=dw2(1:nbrMat)
dw3_vector  (1:nbrMat,iboat)=dw3(1:nbrMat)

dw_b_vector  (1:nbrMat,iboat)=dw_b(1:nbrMat)
dw_c_vector  (1:nbrMat,iboat)=dw_c(1:nbrMat)
dw_l_vector  (1:nbrMat,iboat)=dw_l(1:nbrMat)

labour_cost_vector  (1:nbrMat,iboat)=labour_cost(1:nbrMat)

p4_vector (1:nbrMat,iboat)=p4(1:nbrMat)
dp4_vector(1:nbrMat,iboat)=dp4(1:nbrMat)
p5_vector (1:nbrMat,iboat)=p5(1:nbrMat)
dp5_vector(1:nbrMat,iboat)=dp5(1:nbrMat)
p6_vector (1:nbrMat,iboat)=p6(1:nbrMat)
p7_vector (1:nbrMat,iboat)=p7(1:nbrMat)

p9x_vector (1:nbrMat,iboat)=p9x(1:nbrMat)
dp9x_vector(1:nbrMat,iboat)=dp9x(1:nbrMat)
p9y_vector (1:nbrMat,iboat)=p9y(1:nbrMat)
dp9y_vector(1:nbrMat,iboat)=dp9y(1:nbrMat)

p10_vector (1:nbrMat,iboat)=p10(1:nbrMat)
dp10_vector(1:nbrMat,iboat)=dp10(1:nbrMat)

ber_vector (1:nbrMat,iboat)=ber(1:nbrMat)
bet_vector (1:nbrMat,iboat)=bet(1:nbrMat)

ialr_vector(1:nbrMat,iboat)= ialr(1:nbrMat)
ialt_vector(1:nbrMat,iboat)= ialt(1:nbrMat)

call deallocate_param_materiau()

end
