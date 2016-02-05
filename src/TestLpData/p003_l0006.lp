Minimize

 objective: r'

Subject To

 processes: p' <= 1

 \ processes(i): Step (i) uses at most the given number of processes

 processes(1):
  + t(1,1)
  + t(1,2)
  - p'
  <= 0

 \ resources(i): Step (i) uses at most the given number of resources

 resources(1):
  + m(1,1)
  - r'
  <= 0

 resources(2):
  + m(2,1)
  + m(2,2)
  - r'
  <= 0

 \ consistency_m(i,k): Resource (i,k) can only be available if resource (i+1,k) is available, or a process at (i,k) happend

 consistency_m(1,1): m(1,1) - t(1,1) - m(2,1) <= 0
 consistency_m(1,2):        - t(1,2) - m(2,2) <= -1

 \ consistency_t(i,k): Process (i,k) can only happen if resource (i+1,k-1) is available

 consistency_t(1,2): t(1,2) - m(2,1) <= 0

 \ optimization_a(i,k): Process (i,k) must not happen if resource (i+1,k) is available

 optimization_a(1,1): t(1,1) + m(2,1) <= 1
 optimization_a(1,2): t(1,2) + m(2,2) <= 1

 \ optimization_b(i,k): If process (i,k) happens, it must be used by resource (i,k)

 optimization_b(1,1): t(1,1) - m(1,1) <= 0

 \ optimization_c(i,k): If process (i,k) happens, process (i,k+1) must not happen

 optimization_c(1,1): t(1,1) + t(1,2) <= 1

 \ optimization_d(i,k): If resource (i,k) is available, process (i-1,k+1) must happen or resource (i-1,k) must be available

 optimization_d(2,1): m(2,1) - t(1,2) - m(1,1) <= 0

Bounds

 0 <= p'
 0 <= r'

Integer

 p'
 r'

Binary

 m(1,1)
 m(2,1)
 m(2,2)

 t(1,1)
 t(1,2)

End
