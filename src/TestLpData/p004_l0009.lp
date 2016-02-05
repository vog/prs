Minimize

 objective: r'

Subject To

 processes: p' <= 2

 \ processes(i): Step (i) uses at most the given number of processes

 processes(1):
  + t(1,1)
  + t(1,2)
  - p'
  <= 0

 processes(2):
  + t(2,1)
  + t(2,2)
  + t(2,3)
  - p'
  <= 0

 processes(3):
  + t(3,1)
  + t(3,2)
  + t(3,3)
  + t(3,4)
  - p'
  <= 0

 processes(4):
  + t(4,1)
  + t(4,2)
  + t(4,3)
  + t(4,4)
  + t(4,5)
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

 resources(3):
  + m(3,1)
  + m(3,2)
  + m(3,3)
  - r'
  <= 0

 resources(4):
  + m(4,1)
  + m(4,2)
  + m(4,3)
  + m(4,4)
  - r'
  <= 0

 resources(5):
  + m(5,1)
  + m(5,2)
  + m(5,3)
  + m(5,4)
  + m(5,5)
  - r'
  <= 0

 \ consistency_m(i,k): Resource (i,k) can only be available if resource (i+1,k) is available, or a process at (i,k) happend

 consistency_m(1,1): m(1,1) - t(1,1) - m(2,1) <= 0
 consistency_m(1,2):        - t(1,2) - m(2,2) <= -1
 consistency_m(2,1): m(2,1) - t(2,1) - m(3,1) <= 0
 consistency_m(2,2): m(2,2) - t(2,2) - m(3,2) <= 0
 consistency_m(2,3):        - t(2,3) - m(3,3) <= -1
 consistency_m(3,1): m(3,1) - t(3,1) - m(4,1) <= 0
 consistency_m(3,2): m(3,2) - t(3,2) - m(4,2) <= 0
 consistency_m(3,3): m(3,3) - t(3,3) - m(4,3) <= 0
 consistency_m(3,4):        - t(3,4) - m(4,4) <= -1
 consistency_m(4,1): m(4,1) - t(4,1) - m(5,1) <= 0
 consistency_m(4,2): m(4,2) - t(4,2) - m(5,2) <= 0
 consistency_m(4,3): m(4,3) - t(4,3) - m(5,3) <= 0
 consistency_m(4,4): m(4,4) - t(4,4) - m(5,4) <= 0
 consistency_m(4,5):        - t(4,5) - m(5,5) <= -1

 \ consistency_t(i,k): Process (i,k) can only happen if resource (i+1,k-1) is available

 consistency_t(1,2): t(1,2) - m(2,1) <= 0
 consistency_t(2,2): t(2,2) - m(3,1) <= 0
 consistency_t(2,3): t(2,3) - m(3,2) <= 0
 consistency_t(3,2): t(3,2) - m(4,1) <= 0
 consistency_t(3,3): t(3,3) - m(4,2) <= 0
 consistency_t(3,4): t(3,4) - m(4,3) <= 0
 consistency_t(4,2): t(4,2) - m(5,1) <= 0
 consistency_t(4,3): t(4,3) - m(5,2) <= 0
 consistency_t(4,4): t(4,4) - m(5,3) <= 0
 consistency_t(4,5): t(4,5) - m(5,4) <= 0

 \ optimization_a(i,k): Process (i,k) must not happen if resource (i+1,k) is available

 optimization_a(1,1): t(1,1) + m(2,1) <= 1
 optimization_a(1,2): t(1,2) + m(2,2) <= 1
 optimization_a(2,1): t(2,1) + m(3,1) <= 1
 optimization_a(2,2): t(2,2) + m(3,2) <= 1
 optimization_a(2,3): t(2,3) + m(3,3) <= 1
 optimization_a(3,1): t(3,1) + m(4,1) <= 1
 optimization_a(3,2): t(3,2) + m(4,2) <= 1
 optimization_a(3,3): t(3,3) + m(4,3) <= 1
 optimization_a(3,4): t(3,4) + m(4,4) <= 1
 optimization_a(4,1): t(4,1) + m(5,1) <= 1
 optimization_a(4,2): t(4,2) + m(5,2) <= 1
 optimization_a(4,3): t(4,3) + m(5,3) <= 1
 optimization_a(4,4): t(4,4) + m(5,4) <= 1
 optimization_a(4,5): t(4,5) + m(5,5) <= 1

 \ optimization_b(i,k): If process (i,k) happens, it must be used by resource (i,k)

 optimization_b(1,1): t(1,1) - m(1,1) <= 0
 optimization_b(2,1): t(2,1) - m(2,1) <= 0
 optimization_b(2,2): t(2,2) - m(2,2) <= 0
 optimization_b(3,1): t(3,1) - m(3,1) <= 0
 optimization_b(3,2): t(3,2) - m(3,2) <= 0
 optimization_b(3,3): t(3,3) - m(3,3) <= 0
 optimization_b(4,1): t(4,1) - m(4,1) <= 0
 optimization_b(4,2): t(4,2) - m(4,2) <= 0
 optimization_b(4,3): t(4,3) - m(4,3) <= 0
 optimization_b(4,4): t(4,4) - m(4,4) <= 0

 \ optimization_c(i,k): If process (i,k) happens, process (i,k+1) must not happen

 optimization_c(1,1): t(1,1) + t(1,2) <= 1
 optimization_c(2,1): t(2,1) + t(2,2) <= 1
 optimization_c(2,2): t(2,2) + t(2,3) <= 1
 optimization_c(3,1): t(3,1) + t(3,2) <= 1
 optimization_c(3,2): t(3,2) + t(3,3) <= 1
 optimization_c(3,3): t(3,3) + t(3,4) <= 1
 optimization_c(4,1): t(4,1) + t(4,2) <= 1
 optimization_c(4,2): t(4,2) + t(4,3) <= 1
 optimization_c(4,3): t(4,3) + t(4,4) <= 1
 optimization_c(4,4): t(4,4) + t(4,5) <= 1

 \ optimization_d(i,k): If resource (i,k) is available, process (i-1,k+1) must happen or resource (i-1,k) must be available

 optimization_d(2,1): m(2,1) - t(1,2) - m(1,1) <= 0
 optimization_d(3,1): m(3,1) - t(2,2) - m(2,1) <= 0
 optimization_d(3,2): m(3,2) - t(2,3) - m(2,2) <= 0
 optimization_d(4,1): m(4,1) - t(3,2) - m(3,1) <= 0
 optimization_d(4,2): m(4,2) - t(3,3) - m(3,2) <= 0
 optimization_d(4,3): m(4,3) - t(3,4) - m(3,3) <= 0
 optimization_d(5,1): m(5,1) - t(4,2) - m(4,1) <= 0
 optimization_d(5,2): m(5,2) - t(4,3) - m(4,2) <= 0
 optimization_d(5,3): m(5,3) - t(4,4) - m(4,3) <= 0
 optimization_d(5,4): m(5,4) - t(4,5) - m(4,4) <= 0

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
 m(3,1)
 m(3,2)
 m(3,3)
 m(4,1)
 m(4,2)
 m(4,3)
 m(4,4)
 m(5,1)
 m(5,2)
 m(5,3)
 m(5,4)
 m(5,5)

 t(1,1)
 t(1,2)
 t(2,1)
 t(2,2)
 t(2,3)
 t(3,1)
 t(3,2)
 t(3,3)
 t(3,4)
 t(4,1)
 t(4,2)
 t(4,3)
 t(4,4)
 t(4,5)

End
