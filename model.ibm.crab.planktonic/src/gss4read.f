
*     this function returns u, the field
*     compile with:  
*     gfortran -fPIC --share -o gss4read.so gss4read.f

      SUBROUTINE readfields(fnheader, fn, n1, n2, t, u )

      integer ilo_max,m,n, mn
      integer*2 W
      parameter(ilo_max=32,m=226,n=316,W=1 )
      parameter(mn=m*n)
      integer t(6), n1, n2
      integer layer, savelayer, layerx,isave1,isave2,ksave1,ksave2
      integer i,j,k,npacked, ncomp
      integer*2 ldep(m,n),nlayer(m,n),jc(m,n)

      real u(ilo_max,m,n), dz(ilo_max)
      real packed(mn)
      character*120 fnheader, fn
      character header*80

      fnheader=fnheader(1:n1)
      fn=fn(1:n2)


      open(20,file=fnheader, status='old')
      read (20,'(a)') header
      read (20,*) t,dt,mm,nn,ilo,dz,khor,ndrei
      read(20,*) ldep,nlayer,jc,isave1,isave2,ksave1,ksave2,savelayer
      close(20)

      open(22,file=fn,status='old',form='unformatted')
      
      DO
      do 10 layer=1,savelayer
      read (22,end=12) kstep,t,ncomp,layerx
      read (22,end=12) (packed(i),i=1,ncomp)
        npacked=0
        do 15 k=ksave1,ksave2
        do 15 i=isave1,isave2
          if (and(jc(i,k),W) .eq. W .and. nlayer(i,k) .ge. layer) then
            npacked = npacked+1
            u(layer,i,k)=packed(npacked)
          end if
15      continue
12    continue
10    continue
      end DO
      return
      end
     


