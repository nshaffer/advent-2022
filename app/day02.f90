program day02
  implicit none

  call part1
  call part2

contains

  subroutine part1
    character(3), parameter :: xyz = 'XYZ', abc = 'ABC'

    integer :: u, iostat, score, my_i, opp_i
    character :: my_play, opp_play
    integer :: matrix(3,3) 

    matrix(1,:) = [3, 0, 6] ! rock ties rock, loses to paper, beats scissors
    matrix(2,:) = [6, 3, 0] ! paper beats rock, ties paper, loses to scissors
    matrix(3,:) = [0, 6, 3] ! scissors loses to rock, beats paper, ties scissors
    
    score = 0
    open (newunit=u, file='app/day02-input.txt', action='read')
    do 
       read (u, *, iostat=iostat) opp_play, my_play
       if (iostat /= 0) exit

       opp_i = index(abc, opp_play)
       my_i = index(xyz, my_play)

       score = score + my_i + matrix(my_i, opp_i)
    end do
    close (u)
    print *, score
  end subroutine part1

  subroutine part2
    character(3), parameter :: xyz = 'XYZ', abc = 'ABC'

    integer :: u, iostat, score, verd_i, opp_i
    character :: opp_play, verdict
    integer :: matrix(3,3) 

    matrix(1,:) = [3, 1, 2] ! to lose, play scissors vs rock, rock vs paper, paper vs scissors
    matrix(2,:) = [1, 2, 3] ! to draw, play rock vs rock, etc.
    matrix(3,:) = [2, 3, 1] ! to win, play paper vs rock, scissors vs paper, rock vs scissors
    
    score = 0
    open (newunit=u, file='app/day02-input.txt', action='read')
    do 
       read (u, *, iostat=iostat) opp_play, verdict
       if (iostat /= 0) exit

       opp_i = index(abc, opp_play)
       verd_i = index(xyz, verdict)

       score = score + (verd_i-1)*3 + matrix(verd_i, opp_i)
    end do
    close (u)
    print *, score
    
  end subroutine part2

end program day02
