## How to Contribute to the Algorithm Archive

The *Algorithm Archive* is a community effort to learn about and teach algorithms as a community. As such, it requires a certain level of trust between community members. 
For the most part, the collaboration can be done via github and gitbook, so it is important to understand the basics of [version control](principles_of_code/version_control.md). 
Ideally, all code provided by the community will be submitted via pull requests and discussed accordingly; however, I understand that many individuals are new to collaborative projects, so I will allow submissions by other means (comments, tweets, etc...).
As this project grows in size, it will be harder and harder to facilitate these submissions.
In addition, by submitting in any way other than pull requests, I cannot gaurantee I will be able to list you as a collaborator (though I will certainly do my best to update the `CONTRIBUTERS.md` file accordingly). 

At this point, I am trying to figure out the best way to balance community contributions and text. Right now, I feel comfortable writing the text associated with each algorithm and asking for the community to write individual implementations.
In the future, I might allow other users to write algorithm chapters, but for now let's keep it simple: I'll do the writing, everyone else does the coding.
Now for some specifics on submissions:

1. **Style**: Follow standard style guidelines associated with your language of choice. For C / C++, please use Stroustrup style, with `auto` used rarely or not at all. We have had plenty of discussions about this, which can be found [here](https://github.com/leios/algorithm-archive/issues/18). I will leave the issue open for now in the case that other individuals have more to contribute there. Basically, your code should be readable and understandable to anyone -- especially those who are new to the language. In addition, remember that your code will be displayed in this book, so try to keep to around 80 columns and try to remove any visual clutter. In addition, keep variable names clean and understandable. 
2. **Licensing**: All the code from this project will be under the MIT licence found in `LICENCE.md`; however, the text will be under a Creative Commons Attribution-NonCommercial 4.0 International License.
3. **CONTRIBUTORS.md**: After contributing code, please echo your name to the end of `CONTRIBUTORS.md` with `echo name >> CONTRIBUTORS.md`, and also leave a comment on the top of the code you submitted with your name (or username) saying `// submitted by name`. This way everyone is held accountable and we know who to contact if we want more information.
4. **Building the Algorithm Archive**: If you want to build the Algorithm Archive on your own machine, install gitbook and use `gitbook serve` in the main directory (where `README.md` is). This will provide a local url to go to to view the archive in your browser of choice.

I'll update this list as the project grows. Basically, when you submit code, it will be under an MIT license. Please keep the code clean and put your name (or username) in the `CONTRIBUTORS.md` file. 

Thanks for all the support and considering contributing to the Algorithm Archive!
