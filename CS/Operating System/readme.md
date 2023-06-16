# MFQ Algorithm  

- Multi-level Feedback Queue  
  - Print Gantt-Chart & Table with Process number, Burst Time, Turnaround time, and Waiting time.  
  
  - Under the assumptions,  
  
    ```  
    1. Three Scheduling queues exist  
      - First Queue(Q0): RR scheduling with time-quantum 2  
      - Second Queue(Q1): RR scheduling with time-quantum 4  
      - Third Queue(Q2): FCFS scheduling  
    2. All processes enter to Q0 first  
    3. When process in Qi consumes all time quantums allocated, enter to Qi+1  
    4. Priority:  
      - Q0: Highest  
      - Q2: Lowest  
    5. Scheduling on Higher Priority first  
    6. Uni-processor  
    ```  
    
- `input.txt`: input data format that I supposed  

---  

# VM Management  

- Virtual Management algorithm  
  - MIN algorithm  
  - FIFO algorithm  
  - LFU algorithm  
  - LRU algorithm  
  - WS algorithm   
