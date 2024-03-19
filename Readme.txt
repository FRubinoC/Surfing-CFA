To test the attacks you should configure the "attack.txt" file. The pattern that should be followed to indicate the
tampered sensor is the following:

"""
1-S1
2-S1
3-S1
attack
3-S2
5-S1
attack
exit

"""

With this pattern, for example, two kind of attacks are performed: one with sensors S1,1 S1,2 and S1,3 tampered, and
another one with S2,3 and S1,5 tampered. Two kind of attacks are reported for each attack specified: the simple one 
doesn't consider the transmission of tampered data inside the network and the complex one that tracks the flow of
tampered data inside the network. The attacks will be stored inside the "attacks" folder, which contains two folders,
one with the simple attacks and another one with the complex attacks.

To run a CFA for a surfing program the command that should be executed is:

sh SurfingCFA.sh -f programname