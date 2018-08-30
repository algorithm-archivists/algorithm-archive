# How to Contribute to the Algorithm Archive

The *Algorithm Archive* is an effort to learn about and teach algorithms as a community.
As such, it requires a certain level of trust between community members.
For the most part, the collaboration can be done via GitHub and GitBook, so it is important to understand the basics of [version control](../git_and_version_control/git_and_version_control.md).
Ideally, all code provided by the community will be submitted via pull requests and discussed accordingly; however, I understand that many individuals are new to collaborative projects, so I will allow submissions by other means (comments, tweets, etc...).
As this project grows in size, it will be harder and harder to facilitate these submissions.
In addition, by submitting in any way other than pull requests, I cannot guarantee I will be able to list you as a collaborator (though I will certainly do my best to update the `CONTRIBUTORS.md` file accordingly).

At this point, I am trying to figure out the best way to balance community contributions and text.
Right now, I feel comfortable writing the text associated with each algorithm and asking for the community to write individual implementations.
In the future, I might allow other users to write algorithm chapters, but for now let's keep it simple: I'll do the writing, everyone else does the coding.
Now for some specifics on submissions:

1. **Style**: Follow standard style guidelines associated with your language of choice. For C / C++, please use Stroustrup style, with `auto` used rarely or not at all. We have had plenty of discussions about this, which can be found [here](https://github.com/algorithm-archivists/algorithm-archive/issues/18). I will leave the issue open for now in the case that other individuals have more to contribute there. Basically, your code should be readable and understandable to anyone -- especially those who are new to the language. In addition, remember that your code will be displayed in this book, so try to keep to around 80 columns and try to remove any visual clutter. In addition, keep variable names clean and understandable.
2. **Licensing**: All the code from this project will be under the MIT license found in `LICENSE.md`; however, the text will be under a Creative Commons Attribution-NonCommercial 4.0 International License.
3. **CONTRIBUTORS.md**: After contributing code, please echo your name to the end of `CONTRIBUTORS.md` with `echo name >> CONTRIBUTORS.md`, and also leave a comment on the top of the code you submitted with your name (or username) saying `// submitted by name`. This way everyone is held accountable and we know who to contact if we want more information.
4. **Building the Algorithm Archive**: If you want to build the Algorithm Archive on your own machine, install GitBook and use `gitbook serve` in the main directory (where `README.md` is). This will provide a local URL to go to to view the archive in your browser of choice. Use this server to make sure your version of the Algorithm Archive works cleanly for the chapter you are updating!

For this project, we allow submissions in every language.
To submit code, simply go to the code directory of whatever chapter you want and add a directory for your language of choice.
We use two GitBook plugins to allow users to flip between languages on different algorithms.
One is the theme-api, and the other is the include-codeblock api.
We need the following statements in the markdown file for these to work together:

[import](res/codeblock.txt)

For this example, we are starting the theme-api `method` and importing lines 1-17 from a sample Julia snippet from the code directory.
Note that to standardize the language capitalization schemes, we ask that each language's `sample lang` is the file extension for their code, `cpp` for C++, `hs` for Haskell, etc...
This keeps the title in the theme-api consistent across different languages.
Also note that depending on the algorithm, there might be in-text code snippets that also need to be written.

I'll update this page as the project grows. Basically, when you submit code, it will be under an MIT license. Please keep the code clean and put your name (or username) in the `CONTRIBUTORS.md` file.

If you would like to be a part of the ongoing discussion, please feel free to join our discord server: https://discord.gg/pb976sY.
Thanks for all the support and considering contributing to the Algorithm Archive!
