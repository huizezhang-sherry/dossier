# multiplication works

    Code
      gen_pairwise_paper_grid(tibble::tibble(paper = paper_vec), "paper")
    Output
      # A tibble: 15 x 2
         paper1      paper2     
         <chr>       <chr>      
       1 braga       katsouyanni
       2 braga       ostro      
       3 braga       peel       
       4 braga       schwartz   
       5 braga       zanobetti  
       6 katsouyanni ostro      
       7 katsouyanni peel       
       8 katsouyanni schwartz   
       9 katsouyanni zanobetti  
      10 ostro       peel       
      11 ostro       schwartz   
      12 ostro       zanobetti  
      13 peel        schwartz   
      14 peel        zanobetti  
      15 schwartz    zanobetti  

---

    Code
      gen_pairwise_paper_grid(grid, paper_cols = c("V1", "V2"))
    Output
      # A tibble: 15 x 2
         paper1      paper2     
         <chr>       <chr>      
       1 braga       katsouyanni
       2 braga       ostro      
       3 braga       peel       
       4 braga       schwartz   
       5 braga       zanobetti  
       6 katsouyanni ostro      
       7 katsouyanni peel       
       8 katsouyanni schwartz   
       9 katsouyanni zanobetti  
      10 ostro       peel       
      11 ostro       schwartz   
      12 ostro       zanobetti  
      13 peel        schwartz   
      14 peel        zanobetti  
      15 schwartz    zanobetti  

