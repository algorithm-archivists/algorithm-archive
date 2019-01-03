# Cryptography

Humans have almost always been interested in sending secret messages that only the sender and receiver understand.
The reason for this is obvious: secret messages should remain secret.
The easiest way for this to happen is to talk behind closed doors, but that simply doesn't work if the the sender and receiver are separated by a significant distance.
In this case, they need to rely on a messenger or mailman to send the message.

For simplicity, let's assume they are sending a written letter for the purpose of negotiating war tactics in ancient Greece or Rome.
Obviously, the message can remain secret if both the sender and receiver also trust the messenger; however, what if the messenger is actually an evil spy?
What if the messenger is killed and the letter is stolen?
What if (in an elaborate ruse), some third party slips into the messenger's tent in the dead-of-night and replaces the letter with another one entirely different?

These are all important questions cryptography addresses.

The idea is simple: we procedurally scramble the message we are sending and only provide the unscrambling procedure to trusted parties.
In this way, the message would seem like utter gobbledygook to anyone other than the sender and receiver.
It doesn't matter if the messenger is evil.
They cannot read the message anyway.
It's also fine if the message is replaced, because then the receiver won't be able to properly decode the message and can just ask for another message to be sent (probably on another path with a different messenger).
Unsurprisingly, a very early method of encryption was supposedly developed by Julius Caeser and called the "Caesar Cipher."
Here, every character in the message is replaced by another character based on some pre-defined table or chart that only the sender and receiver have.
The table is created by simply rotating the alphabet by $$n$$ spaces, where $$n$$ is chosen in a discussion between the sender and receiver before-hand.
It is certainly not the most complicated scheme out there, but it is generally the first encryption scheme people come up with when trying to encode secret messages to one another.
Honestly, I remember sending messages back and forth to friends in elementary school, but we would never provide the necessary table to decode the message.
Instead, we would provide enough text that they could find the table themselves from context.
If a bunch of elementary school kids can figure out how to break this encryption scheme, it cannot be too robust.
In fact, it's interesting to see how the field of cryptography has grown since the Caesar cipher was developed.
In the cryptographic literature, there is always a sender, receiver, and eavesdropper.
For some reason beyond my own comprehension, these three people are almost always given the names Alice (sender), Bob (receiver), and Charlie (attacker or eavesdropper).
These names are consistent even with quantum cryptography, so they are here to stay.

In general, there are two different types of encryption: symmetric and asymmetric.
Both of which are described in the following sections.

Cryptographic systems are a cornerstone to modern information technology and lie at the heart of everything from WiFi to online banking.
If an attacker manages to crack modern cryptographic algorithms, they could cause serious damage.
For this reason, it is important to keep a few things in mind:
* Because crypto has become such an advanced field crypto systems should be analyzed by trained professionals and have to go under extensive testing and vetting.
    Meaning whenever possible use a widely accepted cryptography library instead of writing your own cypher.
* Kerckhoffs's principle says that when determining the robustness of a crypto system it should be assumed that the attacker knows the encryption and decryption algorithm. 
    This does not include any pre-shared or secret keys.
* With the advances in technology cryptography often hits its limits.
    Many formerly thought hashing algorithms became obsolete because the computer used to crack them got faster and better.
    Another field that cryptography will have to face is Quantum Computing.
    Quantum Computers will have a big impact on cryptography and especially asymmetric crypto.
    This whole set of problems is summarized in the field of Post-quantum cryptography.

## Symmetric Cryptography

Symmetric cryptography is called symmetric because the key that is used is the same for encrypting and decrypting. 
For this to work Alice and Bob both need the same key, which they have to share before communicating. 
Some examples for symmetric cryptography are:
* **Ceasar Cipher**: Alice and Bob rotate the alphabet by $$n$$ characters and use that as a table to encode and decode their message.
* **Rot13**: This is a special case of the Caeser Cipher where the alphabet is rotated by 13, hence the name "Rot13."
* **Permutation Cipher**: Here you choose a permutation $$\pi$$ (i.e. $$\pi=(3,1,2,4)$$) and reorder the the letters according to that $$\pi$$ which is the key.
* **XOR encryption**: This method works on bit strings and combines the message and a key of equal bit length with the XOR operator.
    To decrypt, simply XOR again with the same key.
* **DES or Data Encryption Standard**: This is a newer encryption algorithm which was standardized in 1977. 
    It has since been deemed insecure and is superseded by AES.
* **AES or Advanced Encryption Standard**: The actual algorithm is called "Rijndael". 
    Like with XOR or DES you generate a bit string (depending on which AES you use 128/192 or 256 bit long) which is your key.
* **Blowfish**: This algorithm also was a good contender for the AES but lost to Rijndael.

This section is currently a work-in-progress, and all of these methods will have corresponding chapters in the near future.

## Asymmetric Cryptography

Asymmetric cryptography is sometimes called "public key cryptography" because Bob and Alice both need a shared public key and a private key they keep to themselves.
This makes these algorithms asymmetric because what is encrypted with the public key can only be decrypted with the private key and vice versa. 
This can be used for a number of different applications, like digital signing, encrypted communication or secretly sharing keys.
For example, if Alice wants to send a message to Bob and Bob wants to make sure the message from Alice was not altered, Alice can encrypt the message with her private key.
If the message is altered (possibly by Charlie), then the message can no longer be decrypted with Alice's public key.
Some examples for public key cryptography:
* **RSA**: This algorithm calculates a public and a private key from two very large primes. 
    It is (hopefully) near impossible to factor the product of two such primes in a feasible amount of time.
* **ECC or Elliptic-curve cryptography**: Here you calculate the private and public key from two points on an elliptic curve. 
    This has the positive side effect that you need smaller numbers than non-ECC algorithms like RSA to achieve the same level of security.

This section is currently a work-in-progress. These methods will also have corresponding chapters in the near future.

## License
The text of this of this chapter is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode) with attribution to Liikt.
The code examples are licensed under the MIT license (found in LICENSE.md).

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by Liikt and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)
