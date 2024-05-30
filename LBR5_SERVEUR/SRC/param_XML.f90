module param_XML

!	Declare an interface block to the routine to be called.

interface
   subroutine wXML_Pop_()
   end subroutine
end interface

interface
	real function wXML_getAttributeDouble_(a)
		character*(*) a	
	end function
end interface

interface
	integer function wXML_TraverseToFirstSon_()
	end function
end interface

interface
	integer function wXML_TraverseToSon_(a)
		character*(*) a	
	end function
end interface

interface 
	integer function wXML_Reader_LoadXML_(a,b)
		character*(*) a
		integer*4 b
	end function
end interface

interface 
	integer function wXML_Reader_SaveXML_(a,b)
		character*(*) a
		integer*4 b
	end function
end interface

interface
	integer function wXML_getAttributeLong_(a)
	    character*(*) a
	end function
end interface

interface
	integer function wXML_Values_(a,taille)
		character*(*) a
		integer*4 taille
	end function
end interface


interface
	integer function wXML_getAttributeString_(a,b,taille)
		character*(*) a
		character*(*) b
		integer*4 taille
	end function
end interface

interface
   integer function wXML_Reader_Init_(documentName)
		character*(*) documentName
   end function
end interface

interface
   integer function wXML_Reader_Terminate_()
   end function
end interface
	
interface
   integer function wXML_TraverseToNextSibling_()
   end function
end interface

interface
   integer function wXML_TraverseToNextFirstSibling_(name)
   character*(*) name
   end function
end interface
	
interface
   subroutine wXML_Push_()
   end subroutine
end interface

interface
	integer function wXML_addSon_(name)
		character*(*) name
	end function
end interface

interface
   integer function wXML_addAttribute_(name, value)
		character*(*) name
		character*(*) value
   end function
end interface

interface
   integer function wXML_addAttributeDouble_(name, value)
	character*(*) name
	double precision value
   end function
end interface

!	Declare a pointer for the dll      
pointer (p,i)

!	Declare pointers for the functions
pointer (q1, wXML_Pop_)
pointer (q2, wXML_getAttributeDouble_)
pointer (q3, wXML_TraverseToFirstSon_)
pointer (q4, wXML_TraverseToSon_)
pointer (q5, wXML_Reader_LoadXML_)
pointer (q6, wXML_getAttributeLong_)
pointer (q7, wXML_Values_)
pointer (q8, wXML_getAttributeString_)
pointer (q9, wXML_Reader_Init_)
pointer (q10, wXML_Reader_Terminate_)
pointer (q11, wXML_TraverseToNextSibling_)
pointer (q12, wXML_TraverseToNextFirstSibling_)
pointer (q13, wXML_Push_)
pointer (q14, wXML_Reader_SaveXML_)
pointer (q15, wXML_addSon_)
pointer (q16, wXML_addAttribute_)
pointer (q17, wXML_addAttributeDouble_)
	
end
