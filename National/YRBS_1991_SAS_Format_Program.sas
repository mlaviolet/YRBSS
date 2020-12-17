
/****************************************************************************************/
/*  This SAS program creates a permanent SAS format library that is used to analyze to  */
/*  analyze the 1991 YRBS dataset.                                                      */
/*                                                                                      */
/*  Change the file location specification from "c:\yrbs1991" to the location where you */
/*  want the format library to be stored before you run this program.  Change the       */
/*  location specification in the "library" statement at the top of the program.        */
/*                                                                                      */
/*  Note: Run "YRBS_1991_SAS_Format_Program.sas" BEFORE you run                         */
/*  "YRBS_1991_SAS_Input_Program.sas" to create the 1991 YRBS dataset.                  */
/****************************************************************************************/

libname library "C:\YRBSS\National";
*libname library "c:\yrbs1991";
proc format library=library;
value $Q1F
' '='Missing'
'1'='12 years old or younger'
'2'='13 years old'
'3'='14 years old'
'4'='15 years old'
'5'='16 years old'
'6'='17 years old'
'7'='18 years or older'
'other'='** DATA ERROR **';
value $Q2F
' '='Missing'
'1'='Female'
'2'='Male'
'other'='** DATA ERROR **';
value $Q3F
' '='Missing'
'1'='9th grade'
'2'='10th grade'
'3'='11th grade'
'4'='12th grade'
'5'='Ungraded or other'
'other'='** DATA ERROR **';
value $Q4F
' '='Missing'
'1'='White - not Hispanic'
'2'='Black - not Hispanic'
'3'='Hispanic'
'4'='Asian or Pacific Islander'
'5'='Native American or Alaskan'
'6'='Other'
'other'='** DATA ERROR **';
value $Q5F
' '='Missing'
'1'='One of the best'
'2'='Far above the middle'
'3'='A little above the middle'
'4'='In the middle'
'5'='A little below the middle'
'6'='Far below the middle'
'7'='Near the bottom'
'other'='** DATA ERROR **';
value $Q6F
' '='Missing'
'1'='Never'
'2'='Rarely'
'3'='Sometimes'
'4'='Most of the time'
'5'='Always'
'other'='** DATA ERROR **';
value $Q7F
' '='Missing'
'1'='0 times'
'2'='1 to 10 times'
'3'='11 to 20 times'
'4'='21 to 39 times'
'5'='40 or more times'
'other'='** DATA ERROR **';
value $Q8F
' '='Missing'
'1'='Did not ride a motorcycle'
'2'='Never'
'3'='Rarely'
'4'='Sometimes'
'5'='Most of the time'
'6'='Always'
'other'='** DATA ERROR **';
value $Q9F
' '='Missing'
'1'='0 times'
'2'='1 to 10 times'
'3'='11 to 20 times'
'4'='21 to 39 times'
'5'='40 or more times'
'other'='** DATA ERROR **';
value $Q10F
' '='Missing'
'1'='Did not ride a bicycle'
'2'='Never'
'3'='Rarely'
'4'='Sometimes'
'5'='Most of the time'
'6'='Always'
'other'='** DATA ERROR **';
value $Q11F
' '='Missing'
'1'='0 times'
'2'='1 time'
'3'='2 or 3 times'
'4'='4 or 5 times'
'5'='6 or more times'
'other'='** DATA ERROR **';
value $Q12F
' '='Missing'
'1'='0 times'
'2'='1 time'
'3'='2 or 3 times'
'4'='4 or 5 times'
'5'='6 or more times'
'other'='** DATA ERROR **';
value $Q13F
' '='Missing'
'1'='Did not go swimming'
'2'='Never'
'3'='Rarely'
'4'='Sometimes'
'5'='Most of the time'
'6'='Always'
'other'='** DATA ERROR **';
value $Q14F
' '='Missing'
'1'='0 days'
'2'='1 day'
'3'='2 or 3 days'
'4'='4 or 5 days'
'5'='6 or more days'
'other'='** DATA ERROR **';
value $Q15F
' '='Missing'
'1'='Did not carry a weapon'
'2'='A handgun'
'3'='Other guns'
'4'='A knife or razor'
'5'='A club, stick, bat, or pipe'
'6'='Some other weapon'
'other'='** DATA ERROR **';
value $Q16F
' '='Missing'
'1'='0 times'
'2'='1 time'
'3'='2 or 3 times'
'4'='4 or 5 times'
'5'='6 or 7 times'
'6'='8 or 9 times'
'7'='10 or 11 times'
'8'='12 or more times'
'other'='** DATA ERROR **';
value $Q17F
' '='Missing'
'1'='Never been in a fight'
'2'='Total stranger'
'3'='Friend or some I know'
'4'='Boyfriend, girlfriend, date'
'5'='Family member'
'6'='Someone not listed above'
'7'='More than one of the above'
'other'='** DATA ERROR **';
value $Q18F
' '='Missing'
'1'='0 times'
'2'='1 time'
'3'='2 or 3 times'
'4'='4 or 5 times'
'5'='6 or more times'
'other'='** DATA ERROR **';
value $Q19F
' '='Missing'
'1'='Yes'
'2'='No'
'other'='** DATA ERROR **';
value $Q20F
' '='Missing'
'1'='Yes'
'2'='No'
'other'='** DATA ERROR **';
value $Q21F
' '='Missing'
'1'='0 times'
'2'='1 time'
'3'='2 or 3 times'
'4'='4 or 5 times'
'5'='6 or more times'
'other'='** DATA ERROR **';
value $Q22F
' '='Missing'
'1'='Did not attempt suicide'
'2'='Yes'
'3'='No'
'other'='** DATA ERROR **';
value $Q23F
' '='Missing'
'1'='Yes'
'2'='No'
'other'='** DATA ERROR **';
value $Q24F
' '='Missing'
'1'='Already tried smoking'
'2'='Yes'
'3'='No'
'other'='** DATA ERROR **';
value $Q25F
' '='Missing'
'1'='Never smoked a cigarette'
'2'='Less than 9 years old'
'3'='9 or 10 years old'
'4'='11 or 12 years old'
'5'='13 or 14 years old'
'6'='15 or 16 years old'
'7'='17 or more years old'
'other'='** DATA ERROR **';
value $Q26F
' '='Missing'
'1'='Yes'
'2'='No'
'other'='** DATA ERROR **';
value $Q27F
' '='Missing'
'1'='Never smoked regularly'
'2'='Less than 9 years old'
'3'='9 or 10 years old'
'4'='11 or 12 years old'
'5'='13 or 14 years old'
'6'='15 or 16 years old'
'7'='17 or more years old'
'other'='** DATA ERROR **';
value $Q28F
' '='Missing'
'1'='0 days'
'2'='1 or 2 days'
'3'='3 to 5 days'
'4'='6 to 9 days'
'5'='10 to 19 days'
'6'='20 to 29 days'
'7'='All 30 days'
'other'='** DATA ERROR **';
value $Q29F
' '='Missing'
'1'='Did not smoke'
'2'='Less than 1 per day'
'3'='1 per day'
'4'='2 to 5 per day'
'5'='6 to 10 per day'
'6'='11 to 20 per day'
'7'='More than 20 per day'
'other'='** DATA ERROR **';
value $Q30F
' '='Missing'
'1'='Did not smoke'
'2'='Yes'
'3'='No'
'other'='** DATA ERROR **';
value $Q31F
' '='Missing'
'1'='No'
'2'='Yes, chewing tobacco only'
'3'='Yes, snuff only'
'4'='Yes, both tobacco and snuff'
'other'='** DATA ERROR **';
value $Q32F
' '='Missing'
'1'='Never drank alcohol'
'2'='Less than 9 years old'
'3'='9 to 10 years old'
'4'='11 to 12 years old'
'5'='13 to 14 years old'
'6'='15 to 16 years old'
'7'='17 or more years old'
'other'='** DATA ERROR **';
value $Q33F
' '='Missing'
'1'='0 days'
'2'='1 or 2 days'
'3'='3 to 9 days'
'4'='10 to 19 days'
'5'='20 to 39 days'
'6'='40 to 99 days'
'7'='100 or more days'
'other'='** DATA ERROR **';
value $Q34F
' '='Missing'
'1'='0 days'
'2'='1 or 2 days'
'3'='3 to 5 days'
'4'='6 to 9 days'
'5'='10 to 19 days'
'6'='20 to 29 days'
'7'='All 30 days'
'other'='** DATA ERROR **';
value $Q35F
' '='Missing'
'1'='0 days'
'2'='1 day'
'3'='2 days'
'4'='3 to 5 days'
'5'='6 to 9 days'
'6'='10 to 19 days'
'7'='20 or more days'
'other'='** DATA ERROR **';
value $Q36F
' '='Missing'
'1'='Never tried marijuana'
'2'='Less than 9 years old'
'3'='9 or 10 years old'
'4'='11 or 12 years old'
'5'='13 or 14 years old'
'6'='15 or 16 years old'
'7'='17 or more years old'
'other'='** DATA ERROR **';
value $Q37F
' '='Missing'
'1'='0 times'
'2'='1 or 2 times'
'3'='3 to 9 times'
'4'='10 to 19 times'
'5'='20 to 39 times'
'6'='40 to 99 times'
'7'='100 or more times'
'other'='** DATA ERROR **';
value $Q38F
' '='Missing'
'1'='0 times'
'2'='1 or 2 times'
'3'='3 to 9 times'
'4'='10 to 19 times'
'5'='20 to 39 times'
'6'='40 or more times'
'other'='** DATA ERROR **';
value $Q39F
' '='Missing'
'1'='Never tried cocaine'
'2'='Less than 9 years old'
'3'='9 or 10 years old'
'4'='11 or 12 years old'
'5'='13 or 14 years old'
'6'='15 or 16 years old'
'7'='17 or more years old'
'other'='** DATA ERROR **';
value $Q40F
' '='Missing'
'1'='0 times'
'2'='1 or 2 times'
'3'='3 to 9 times'
'4'='10 to 19 times'
'5'='20 to 39 times'
'6'='40 or more times'
'other'='** DATA ERROR **';
value $Q41F
' '='Missing'
'1'='0 times'
'2'='1 or 2 times'
'3'='3 to 9 times'
'4'='10 to 19 times'
'5'='20 to 39 times'
'6'='40 or more times'
'other'='** DATA ERROR **';
value $Q42F
' '='Missing'
'1'='0 times'
'2'='1 or 2 times'
'3'='3 to 9 times'
'4'='10 to 19 times'
'5'='20 to 39 times'
'6'='40 or more times'
'other'='** DATA ERROR **';
value $Q43F
' '='Missing'
'1'='0 times'
'2'='1 or 2 times'
'3'='3 to 9 times'
'4'='10 to 19 times'
'5'='20 to 39 times'
'6'='40 or more times'
'other'='** DATA ERROR **';
value $Q44F
' '='Missing'
'1'='0 times'
'2'='1 or 2 times'
'3'='3 to 9 times'
'4'='10 to 19 times'
'5'='20 to 39 times'
'6'='40 or more times'
'other'='** DATA ERROR **';
value $Q45F
' '='Missing'
'1'='Yes'
'2'='No'
'other'='** DATA ERROR **';
value $Q46F
' '='Missing'
'1'='Yes'
'2'='No'
'3'='Not sure'
'other'='** DATA ERROR **';
value $Q47F
' '='Missing'
'1'='Yes'
'2'='No'
'3'='Not sure'
'other'='** DATA ERROR **';
value $Q48F
' '='Missing'
'1'='Yes'
'2'='No'
'other'='** DATA ERROR **';
value $Q49F
' '='Missing'
'1'='Never had sexual intercourse'
'2'='Less than 12 years old'
'3'='12 years old'
'4'='13 years old'
'5'='14 years old'
'6'='15 years old'
'7'='16 years old'
'8'='17 or more years old'
'other'='** DATA ERROR **';
value $Q50F
' '='Missing'
'1'='Never had sexual intercourse'
'2'='1 person'
'3'='2 people'
'4'='3 people'
'5'='4 people'
'6'='5 people'
'7'='6 or more people'
'other'='** DATA ERROR **';
value $Q51F
' '='Missing'
'1'='Never had sexual intercourse'
'2'='None in past 3 months'
'3'='1 person'
'4'='2 people'
'5'='3 people'
'6'='4 people'
'7'='5 people'
'8'='6 or more people'
'other'='** DATA ERROR **';
value $Q52F
' '='Missing'
'1'='Never had sexual intercourse'
'2'='Yes'
'3'='No'
'other'='** DATA ERROR **';
value $Q53F
' '='Missing'
'1'='Never had sexual intercourse'
'2'='Yes'
'3'='No'
'other'='** DATA ERROR **';
value $Q54F
' '='Missing'
'1'='Never had sexual intercourse'
'2'='No method'
'3'='Birth control pills'
'4'='Condoms'
'5'='Withdrawal'
'6'='Some other method'
'7'='Not sure'
'other'='** DATA ERROR **';
value $Q55F
' '='Missing'
'1'='0 times'
'2'='1 time'
'3'='2 or more times'
'4'='Not sure'
'other'='** DATA ERROR **';
value $Q56F
' '='Missing'
'1'='Yes'
'2'='No'
'other'='** DATA ERROR **';
value $Q57F
' '='Missing'
'1'='Very underweight'
'2'='Slightly underweight'
'3'='About the right weight'
'4'='Slightly overweight'
'5'='Very overweight'
'other'='** DATA ERROR **';
value $Q58F
' '='Missing'
'1'='Lose weight'
'2'='Gain weight'
'3'='Stay the same weight'
'4'='Not trying to do anything'
'other'='** DATA ERROR **';
value $Q59F
' '='Missing'
'1'='Did not do anything'
'2'='Dieted'
'3'='Exercised'
'4'='Exercised and dieted'
'5'='Other method'
'other'='** DATA ERROR **';
value $Q60F
' '='Missing'
'1'='Did not do anything'
'2'='Made myself vomit'
'3'='Took diet pills'
'4'='Vomiting and diet pills'
'5'='Other method'
'other'='** DATA ERROR **';
value $Q61F
' '='Missing'
'1'='No'
'2'='Yes, once only'
'3'='Yes, twice or more'
'other'='** DATA ERROR **';
value $Q62F
' '='Missing'
'1'='No'
'2'='Yes, once only'
'3'='Yes, twice or more'
'other'='** DATA ERROR **';
value $Q63F
' '='Missing'
'1'='No'
'2'='Yes, once only'
'3'='Yes, twice or more'
'other'='** DATA ERROR **';
value $Q64F
' '='Missing'
'1'='No'
'2'='Yes, once only'
'3'='Yes, twice or more'
'other'='** DATA ERROR **';
value $Q65F
' '='Missing'
'1'='No'
'2'='Yes, once only'
'3'='Yes, twice or more'
'other'='** DATA ERROR **';
value $Q66F
' '='Missing'
'1'='No'
'2'='Yes, once only'
'3'='Yes, twice or more'
'other'='** DATA ERROR **';
value $Q67F
' '='Missing'
'1'='No'
'2'='Yes, once only'
'3'='Yes, twice or more'
'other'='** DATA ERROR **';
value $Q68F
' '='Missing'
'1'='0 days'
'2'='1 day'
'3'='2 days'
'4'='3 days'
'5'='4 days'
'6'='5 days'
'7'='6 days'
'8'='7 days'
'other'='** DATA ERROR **';
value $Q69F
' '='Missing'
'1'='0 days'
'2'='1 day'
'3'='2 days'
'4'='3 days'
'5'='4 days'
'6'='5 days'
'7'='6 days'
'8'='7 days'
'other'='** DATA ERROR **';
value $Q70F
' '='Missing'
'1'='0 days'
'2'='1 day'
'3'='2 days'
'4'='3 days'
'5'='4 days'
'6'='5 days'
'7'='6 days'
'8'='7 days'
'other'='** DATA ERROR **';
value $Q71F
' '='Missing'
'1'='Yes'
'2'='No'
'other'='** DATA ERROR **';
value $Q72F
' '='Missing'
'1'='0 days'
'2'='1 day'
'3'='2 days'
'4'='3 days'
'5'='4 days'
'6'='5 days'
'other'='** DATA ERROR **';
value $Q73F
' '='Missing'
'1'='Do not take PE'
'2'='Less than 10 minutes'
'3'='10 to 20 minutes'
'4'='21 to 30 minutes'
'5'='More than 30 minutes'
'other'='** DATA ERROR **';
value $Q74F
' '='Missing'
'1'='None'
'2'='1 team'
'3'='2 teams'
'4'='3 or more teams'
'other'='** DATA ERROR **';
value $Q75F
' '='Missing'
'1'='None'
'2'='1 team'
'3'='2 teams'
'4'='3 or more teams'
'other'='** DATA ERROR **';
value $GREGH
' '='Missing'
'1'='Northeast'
'2'='Midwest'
'3'='South'
'4'='West'
'other'='** DATA ERROR **';
run;
