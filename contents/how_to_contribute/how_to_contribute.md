# How to Contribute to the Algorithm Archive

The *Algorithm Archive* is an effort to learn about and teach algorithms as a community.
As such, it requires a certain level of trust between community members.
For specific details on how to contribute, please consult the [How to Contribute guide](https://github.com/algorithm-archivists/algorithm-archive/wiki/How-to-Contribute).
If you are having trouble with git and version control, please also check out [this video series](https://www.youtube.com/playlist?list=PL5NSPcN6fRq2vwgdb9noJacF945CeBk8x) with more details.

In addition, we also have an [FAQ](https://github.com/algorithm-archivists/algorithm-archive/wiki/FAQ) and a [code style guide](https://github.com/algorithm-archivists/algorithm-archive/wiki/Code-style-guide), which is currently being written for all languages submitted to the Algorithm Archive so far.

Currently, we are not accepting chapter submissions; however, we will allow for this in the near future. 
For now, here are the basics for submitting code to the Algorithm Archive:

1. **Style**: We are developing a [code style guide](https://github.com/algorithm-archivists/algorithm-archive/wiki/Code-style-guide) for all the languages in the Algorithm Archive. For the most part, follow standard style guidelines associated with your language of choice. Your code should be readable and understandable to anyone -- especially those who are new to the language. In addition, remember that your code will be displayed in this book, so try to keep to around 80 columns, try to remove any visual clutter, and keep variable names clean and understandable.
2. **Licensing**: All the code from this project will be under the MIT license found in `LICENSE.md`; however, the text will be under a Creative Commons Attribution-NonCommercial 4.0 International License.
3. **CONTRIBUTORS.md**: After contributing code, please echo your name to the end of `CONTRIBUTORS.md` with `echo "- name" >> CONTRIBUTORS.md`.
4. **Building the Algorithm Archive**: Before every submission, you should build the Algorithm Archive on your own machine. To do this, install GitBook and use `gitbook install` and then `gitbook serve` in the main directory (where `README.md` is). This will provide a local URL to go to to view the archive in your browser of choice. Use this server to make sure your version of the Algorithm Archive works cleanly for the chapter you are updating!

To submit code, simply go to the `code/` directory of whatever chapter you want and add another directory for your language of choice.

We use two GitBook plugins to allow users to flip between languages on different algorithms.
One is the theme-api, and the other is the include-codeblock api.
We need the following statements in the markdown file for these to work together:

[import](res/codeblock.txt)

For this example, we are starting the theme-api `method` and importing lines 1-17 from a sample Julia snippet from the code directory.
Note that to standardize the language capitalization schemes, we ask that each language's `sample lang` is the file extension for their code, `cpp` for C++, `hs` for Haskell, etc.
This keeps the title in the theme-api consistent across different languages.
Also note that depending on the algorithm, there might be in-text code snippets that also need to be written.

I'll update this page as the project grows. 
If you would like to be a part of the ongoing discussion, please feel free to join our discord server: https://discord.gg/pb976sY.
Thanks for all the support and considering contributing to the Algorithm Archive!

## License

##### Code Examples

The code examples are licensed under the MIT license (found in [LICENSE.md](https://github.com/algorithm-archivists/algorithm-archive/blob/master/LICENSE.md)).

##### Text

The text of this chapter was written by [James Schloss](https://github.com/leios) and is licensed under the [Creative Commons Attribution-ShareAlike 4.0 International License](https://creativecommons.org/licenses/by-sa/4.0/legalcode).

[<p><img  class="center" src="../cc/CC-BY-SA_icon.svg" /></p>](https://creativecommons.org/licenses/by-sa/4.0/)

##### Pull Requests

After initial licensing ([#560](https://github.com/algorithm-archivists/algorithm-archive/pull/560)), the following pull requests have modified the text or graphics of this chapter:
- none
