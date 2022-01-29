



alpha = 0.01
beta_1 = 0.9
beta_2= 0.999
epsilon = 1e-8


w_0=0
m_wt=0
m_bt=0
v_wt=0
v_bt=0
t=0

grad_w<-function(w){
  y =  2*w-4
}
 

for(i in 1:500){
  t = t+1
  # m
  #------------------------------------------------------------
  w_t = grad_w(w_0)
  
  m_wt = beta_1 * m_wt + (1-beta_1) * w_t
  v_wt = beta_2 * v_wt + (1-beta_2) * (w_t*w_t)
  
  m_wcap = m_wt/(1-(beta_1^t))
  v_wcap = v_wt/(1-(beta_2^t))
  
  
  w_0 = w_0 - (alpha*m_wcap)/(sqrt(v_wcap)+epsilon)
  
  
  
 
  
  
  print(list(w=w_0))
  
}



