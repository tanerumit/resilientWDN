Minimize
 obj:  8.3246309 Tilbury_to_Manchester + 0 Tilbury_to_Tilbury + 10.3362814 Tilbury_to_Paris + 39.1826328 Tilbury_to_Madrid + 25.8743436 Tilbury_to_Munich + 83.734995 Tilbury_to_Reykjavik + 56.2274005 Tilbury_to_Sofia + 17.2824468 Tilbury_to_Frankfurt + 28.8817777 Tilbury_to_Prague + 44.6307031 Florence_to_Manchester + 36.5168209 Florence_to_Tilbury + 26.4074785 Florence_to_Paris + 38.6543527 Florence_to_Madrid + 14.8013005 Florence_to_Munich + 94.779894 Florence_to_Reykjavik + 32.1722787 Florence_to_Sofia + 22.1813751 Florence_to_Frankfurt + 23.693714 Florence_to_Prague + 42.1463966 Barcelona_to_Manchester + 34.0324915 Barcelona_to_Tilbury + 23.7480557 Barcelona_to_Paris + 14.3096375 Barcelona_to_Madrid + 31.3500313 Barcelona_to_Munich + 103.4679708 Barcelona_to_Reykjavik + 54.520091 Barcelona_to_Sofia + 30.4529238 Barcelona_to_Frankfurt + 39.3691304 Barcelona_to_Prague 
Subject To 
  c1: Tilbury_to_Manchester + Tilbury_to_Tilbury + Tilbury_to_Paris + Tilbury_to_Madrid + Tilbury_to_Munich + Tilbury_to_Reykjavik + Tilbury_to_Sofia + Tilbury_to_Frankfurt + Tilbury_to_Prague <= 100 
  c2: Tilbury_to_Manchester + Florence_to_Manchester + Barcelona_to_Manchester = 375 
  c3: Florence_to_Manchester + Florence_to_Tilbury + Florence_to_Paris + Florence_to_Madrid + Florence_to_Munich + Florence_to_Reykjavik + Florence_to_Sofia + Florence_to_Frankfurt + Florence_to_Prague <= 100 
  c4: Tilbury_to_Tilbury + Florence_to_Tilbury + Barcelona_to_Tilbury = 200 
  c5: Barcelona_to_Manchester + Barcelona_to_Tilbury + Barcelona_to_Paris + Barcelona_to_Madrid + Barcelona_to_Munich + Barcelona_to_Reykjavik + Barcelona_to_Sofia + Barcelona_to_Frankfurt + Barcelona_to_Prague <= 100 
  c6: Tilbury_to_Paris + Florence_to_Paris + Barcelona_to_Paris = 250 
  c7: Tilbury_to_Manchester + Tilbury_to_Tilbury + Tilbury_to_Paris + Tilbury_to_Madrid + Tilbury_to_Munich + Tilbury_to_Reykjavik + Tilbury_to_Sofia + Tilbury_to_Frankfurt + Tilbury_to_Prague <= 100 
  c8: Tilbury_to_Madrid + Florence_to_Madrid + Barcelona_to_Madrid = 150 
  c9: Florence_to_Manchester + Florence_to_Tilbury + Florence_to_Paris + Florence_to_Madrid + Florence_to_Munich + Florence_to_Reykjavik + Florence_to_Sofia + Florence_to_Frankfurt + Florence_to_Prague <= 100 
  c10: Tilbury_to_Munich + Florence_to_Munich + Barcelona_to_Munich = 100 
  c11: Barcelona_to_Manchester + Barcelona_to_Tilbury + Barcelona_to_Paris + Barcelona_to_Madrid + Barcelona_to_Munich + Barcelona_to_Reykjavik + Barcelona_to_Sofia + Barcelona_to_Frankfurt + Barcelona_to_Prague <= 100 
  c12: Tilbury_to_Reykjavik + Florence_to_Reykjavik + Barcelona_to_Reykjavik = 125 
  c13: Tilbury_to_Manchester + Tilbury_to_Tilbury + Tilbury_to_Paris + Tilbury_to_Madrid + Tilbury_to_Munich + Tilbury_to_Reykjavik + Tilbury_to_Sofia + Tilbury_to_Frankfurt + Tilbury_to_Prague <= 100 
  c14: Tilbury_to_Sofia + Florence_to_Sofia + Barcelona_to_Sofia = 25 
  c15: Florence_to_Manchester + Florence_to_Tilbury + Florence_to_Paris + Florence_to_Madrid + Florence_to_Munich + Florence_to_Reykjavik + Florence_to_Sofia + Florence_to_Frankfurt + Florence_to_Prague <= 100 
  c16: Tilbury_to_Frankfurt + Florence_to_Frankfurt + Barcelona_to_Frankfurt = 47 
  c17: Barcelona_to_Manchester + Barcelona_to_Tilbury + Barcelona_to_Paris + Barcelona_to_Madrid + Barcelona_to_Munich + Barcelona_to_Reykjavik + Barcelona_to_Sofia + Barcelona_to_Frankfurt + Barcelona_to_Prague <= 100 
  c18: Tilbury_to_Prague + Florence_to_Prague + Barcelona_to_Prague = 111 
End