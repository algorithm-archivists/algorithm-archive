# Cryptography

For a long time humans wanted to send secret messages that only the reciever understands. 
The first encryption algorithms go back to the ancient greeks and romans. 
One of the most well known encryptions is the so called "Caeser Cipher" which was supposedly used by Julius Caeser. 
In general there are two different principle of encryption, symmetric and asymmetric encryption. 
To explain the two principles I have to introduce you to two people. Enter Alice and Bob. 
These two names are very common when reading about cryptography.
A third and (usually) evil person is also needed and that person is called Charlie. 
The way these three people are set up are so that Alice and Bob want to exchange messages without Charlie knowing what they said.

## General principles of Cryptography

* Don't underestimate the attacker. 
This is important because you don't know what resources an attacker has. 
It is always better to assume that they have a considerable amount of knowledge and computing power of the current global computing power.
* If you devise a new algorithm let a cryptanalyst meassure a robustness of your algorithm. 
Do *NOT* claim it is strong (unless you have the knowledge and proofs needed) without talking to a specialist because only they, if at all, should be able to determine whether the algorithm is good or not since cryptography and security go hand in hand.
* Kerckhoffs's principle says that when determing the robustness of a cryptosystem it should be assumed that the attacker knows the encryption and decryption algorithm. 
This does not include any pre-shared or secret keys.
* Adding complications don't necessarily make the algorithm better or safer.
* Always account for wrong design, implementation and usage of cryptosystems.
A good example for a wrong implementation is WPA2 (the algorithm which encrypts WiFi traffic). This algorithm was mathematically proven to be safe yet an error in the implementaion allowed for the "Krack" attack.   

## Symmetric Cryptography

Symmetric cryptography is called symmetric because the way you encrypt a message is the same as the way to decrypt a message. 
For this to work Alice and Bob both need the same key, which they have to share before communicating. 
Some examples for symmetric cryptography are:
* The afore mentioned Caeser Cipher. Here Alice and Bob have to know the rotation of the alphabet which is the key in this algorithm.
* Rot13 is a special case of the Caeser Cipher. The alphabet gets rotated by 13 (that is why it's called Rot13) so it would be Caeser with the key of 13.
* Permutation Cipher. Here you choose a permutation $$\pi$$ (i.e. $$\pi=(3,1,2,4)$$) and reorder the the letters according to that $$\pi$$ which is the key.
* XOR encryption. Here you generate a bit string which is exactly as long as the message you want to encrypt and just XOR them together.
* DES or Data Encryption Standard. This is a newer encryption algorithm which was standardized in 1977. 
It has since been deemed unsecure and is superseded by AES.
* AES or Advanced Encryption Standard. The actual algorithm is called "Rijndael". 
Like with XOR or DES you generate a bit string (depending on which AES you use 128/192 or 256 bit long) which is your key.
* Blowfish. This algorithm also was a good contender for the AES but lost to Rijndael.

## Asymmetric Cryptography

Asymmetric Cryptography is sometimes called "Public key cryptography" because Bob and Alice both need a public and a private key of which they only share the public key. 
This makes these algorithms asymmetric because what is encrypted with the public key can only be decrypted with the private key and vice versa. 
This has numerous fields of application. It is not only used for encrypting messages, but also for digital signing.
Digital signing is used to make sure that the recieved message actually originates from the person who claims to have written it and you can also make sure nothing got altered. 
So for example Alice wants to send a message to Bob and Bob wants to make sure the message is actually from Alice and arrived the way it was send out. 
For that Alice encrypts the message with her private key. 
This is *NOT* so that Charlie can't read the message, since both Bob and Charlie have Alices private key.
What both of them don't have is the private key. 
So Charlie can't decrypt, alter and encrypt the message again without Bob noticing it, because it wouldn't decrypt anymore with Alices public key. 
Some examples for public key cryptography:
* RSA. This algorithm calculates a public and a private key from two very large primes. It is (hopefully) near impossible to factor the product of two such primes in a feasable amount of time.
* ECC or Elliptic-curve cryptography. Here you calulate the private and public key from two points on an elliptic curve. This has the positive side effect that you need smaller numbers than non-ECC algorithms like RSA to achieve the same level of security.
