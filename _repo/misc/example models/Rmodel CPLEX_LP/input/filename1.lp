Maximize
 obj: S1_to_D1 + S1_to_D2 + S1_to_D3 + S1_to_D4 + S1_to_D5 + S1_to_D6 + S2_to_D1 + S2_to_D2 + S2_to_D3 + S2_to_D4 + S2_to_D5 + S2_to_D6 + S3_to_D1 + S3_to_D2 + S3_to_D3 + S3_to_D4 + S3_to_D5 + S3_to_D6 
Subject To 
  c1: S1_to_D1 + S1_to_D2 + S1_to_D3 + S1_to_D4 + S1_to_D5 + S1_to_D6 <= 100 
  c2: S2_to_D1 + S2_to_D2 + S2_to_D3 + S2_to_D4 + S2_to_D5 + S2_to_D6 <= 100 
  c3: S3_to_D1 + S3_to_D2 + S3_to_D3 + S3_to_D4 + S3_to_D5 + S3_to_D6 <= 100 
  c4: S1_to_D1 + S2_to_D1 + S3_to_D1 <= 180 
  c5: S1_to_D2 + S2_to_D2 + S3_to_D2 <= 140 
  c6: S1_to_D3 + S2_to_D3 + S3_to_D3 <= 20 
  c7: S1_to_D4 + S2_to_D4 + S3_to_D4 <= 300 
  c8: S1_to_D5 + S2_to_D5 + S3_to_D5 <= 200 
  c9: S1_to_D6 + S2_to_D6 + S3_to_D6 <= 10 
End