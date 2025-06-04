# generate paper grid

    Code
      gen_paper_grid(paper_vec)
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
      gen_paper_grid(grid, cols = c("V1", "V2"))
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
      gen_paper_grid(grid, cols = c("V1", "V2"), new_names = c("V1", "V2"))
    Output
      # A tibble: 15 x 2
         V1          V2         
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

# workflow

    Code
      tbl_df
    Output
      # A tibble: 35 x 8
         paper       variable            type   model method parameter reason decision
         <chr>       <chr>               <chr>  <chr> <chr>  <chr>     <chr>  <chr>   
       1 braga       barometric_pressure param~ gene~ LOESS  smoothin~ to mi~ <NA>    
       2 braga       barometric_pressure spati~ gene~ <NA>   <NA>      to al~ chosen ~
       3 braga       humidity            param~ gene~ LOESS  smoothin~ to mi~ <NA>    
       4 braga       humidity            spati~ gene~ <NA>   <NA>      to al~ chosen ~
       5 braga       temperature         param~ gene~ LOESS  smoothin~ to mi~ <NA>    
       6 braga       temperature         spati~ gene~ <NA>   <NA>      to al~ chosen ~
       7 braga       time                param~ gene~ LOESS  smoothin~ to el~ <NA>    
       8 braga       time                spati~ gene~ <NA>   <NA>      seaso~ chosen ~
       9 katsouyanni humidity            param~ gene~ LOESS  smoothin~ to mi~ <NA>    
      10 katsouyanni humidity            tempo~ gene~ <NA>   <NA>      <NA>   same da~
      # i 25 more rows

---

    Code
      count_variable_type(tbl_df)
    Output
      # A tibble: 11 x 3
         variable            type         .n
         <chr>               <chr>     <int>
       1 temperature         parameter     6
       2 time                parameter     6
       3 humidity            parameter     4
       4 temperature         temporal      4
       5 PM                  temporal      3
       6 humidity            temporal      3
       7 time                spatial       2
       8 barometric_pressure parameter     1
       9 barometric_pressure spatial       1
      10 humidity            spatial       1
      11 temperature         spatial       1

---

    Code
      df
    Output
      # A tibble: 26 x 8
         paper       variable    type      model      method parameter reason decision
         <chr>       <chr>       <chr>     <chr>      <chr>  <chr>     <chr>  <chr>   
       1 braga       humidity    parameter generaliz~ LOESS  smoothin~ to mi~ <NA>    
       2 braga       temperature parameter generaliz~ LOESS  smoothin~ to mi~ <NA>    
       3 braga       time        parameter generaliz~ LOESS  smoothin~ to el~ <NA>    
       4 katsouyanni humidity    parameter generaliz~ LOESS  smoothin~ to mi~ <NA>    
       5 katsouyanni humidity    temporal  generaliz~ <NA>   <NA>      <NA>   same da~
       6 katsouyanni temperature parameter generaliz~ LOESS  smoothin~ to mi~ <NA>    
       7 katsouyanni temperature temporal  generaliz~ <NA>   <NA>      <NA>   same da~
       8 katsouyanni time        parameter generaliz~ LOESS  smoothin~ to mi~ <NA>    
       9 ostro       PM          temporal  Poisson r~ <NA>   <NA>      previ~ 2-day a~
      10 ostro       humidity    parameter Poisson r~ smoot~ smoothin~ <NA>   3       
      # i 16 more rows

---

    Code
      count_paper_decisions(df)
    Output
              paper .n
      1       ostro 11
      2 katsouyanni  8
      3    schwartz  8
      4   zanobetti  8
      5       braga  6
      6        peel  6

---

    Code
      paper_df
    Output
      # A tibble: 26 x 8
         paper       variable    type      model      method parameter reason decision
         <chr>       <chr>       <chr>     <chr>      <chr>  <chr>     <chr>  <chr>   
       1 braga       humidity    parameter generaliz~ LOESS  smoothin~ to mi~ <NA>    
       2 braga       temperature parameter generaliz~ LOESS  smoothin~ to mi~ <NA>    
       3 braga       time        parameter generaliz~ LOESS  smoothin~ to el~ <NA>    
       4 katsouyanni humidity    parameter generaliz~ LOESS  smoothin~ to mi~ <NA>    
       5 katsouyanni humidity    temporal  generaliz~ <NA>   <NA>      <NA>   same da~
       6 katsouyanni temperature parameter generaliz~ LOESS  smoothin~ to mi~ <NA>    
       7 katsouyanni temperature temporal  generaliz~ <NA>   <NA>      <NA>   same da~
       8 katsouyanni time        parameter generaliz~ LOESS  smoothin~ to mi~ <NA>    
       9 ostro       PM          temporal  Poisson r~ <NA>   <NA>      previ~ 2-day a~
      10 ostro       humidity    parameter Poisson r~ smoot~ smoothin~ <NA>   3       
      # i 16 more rows

---

    Code
      count_paper_pair_decisions(paper_df)
    Output
      # A data frame: 15 x 3
         paper1      paper2         .n
         <chr>       <chr>       <int>
       1 braga       katsouyanni     6
       2 braga       ostro           4
       3 braga       peel            2
       4 braga       schwartz        4
       5 braga       zanobetti       6
       6 katsouyanni ostro           6
       7 katsouyanni peel            3
       8 katsouyanni schwartz        4
       9 katsouyanni zanobetti       8
      10 ostro       peel            6
      11 ostro       schwartz        7
      12 ostro       zanobetti       6
      13 peel        schwartz        5
      14 peel        zanobetti       3
      15 schwartz    zanobetti       4

---

    Code
      embed_df
    Output
      $tokens
      list()
      
      $texts
      list()
      
      $word_types
      list()
      

