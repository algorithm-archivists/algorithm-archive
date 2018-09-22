# Git and Version Control

I am a fan of open-source software. It allows users to see inside the code running on their system and mess around with it if they like.
Unlike proprietary software, open source software allows any user to learn the entire codebase from the ground up, and that's an incredibly exciting prospect!
More than that, open-source development breeds strong communities of like-minded individuals who work together to solve problems they care about.
At least in my case, different open-source communities inspired me to code in my free time.
It taught me that programming is more than a simple series of instructions for a computer.
More than anything, though, open-source software development taught me about how to work with others and overcome petty squabbles, because if there's one thing any open-source community is known for, it's petty squabbling.

It might be because of my appreciation of large-scale software development that I never questioned the utility of version control.
If there are a couple hundred people all contributing source code to the same place, there has to be some way to control all the different codebases each individual has on their own machine.
Even if I have no collaborators, version control is a way to make sure my laptop and work machine both have the same code without having to transfer a USB stick back and forth.
That said, at some point in my life, I became a physics researcher.
This meant that I wrote code to solve physics problems with a small team. The problem was that even though I was using version control, the rest of my team was not.

This was frustrating.

I would hear my labmates say things like, "I rewrote my code last night and now nothing works, but I already saved over my previous version, so I'll just work with what I have."
Or, "I'm writing a paper with my boss. We are using Dropbox and upload files with slightly different names after we modify them. Right now, we are on paper_78c."
The point is: version control exists to control different versions of software (and other documents).
If you are writing code, it exists as a way to quickly save what you have before making largescale modifications.
It also allows individuals to collaborate on a larger scale by providing necessary tools to merge work created by different individuals into a single, cohesive story.

No matter how you look at it, version control is a useful and necessary tool to collaborate with other programmers and is definitely worth discussing in depth.
Though many version control systems exist, for now we will focus on git, simply because it is incredibly popular and this book is hosted both on GitHub and GitBook.
We hope to discuss other version control methods and strengthen this tutorial on git in the future; however, this book is meant as an archive of algorithms, not as an introduction to version control or best software practices.
Though discussions like these are useful, we must be careful not to get too far out-of-scope.
For now, this tutorial is simply meant as a quick way to kickstart our community into using git and collaborating more effectively with each other (specifically on this book).

I feel like this introduction may have been a little too long. Let me know what you think! Regardless, now it's time to talk about git!

### *Git*ting started!

I suppose let's start simply: git manages different versions of code available on different machines and from different locations.
When using git, there will be a local copy of a repository of code that may or may not be up-to-date with a copy of the code repository in some remote location.

Now, there is an easy way, a hard way, and an impossibly complicated way to use git.
We'll be walking through the easy and hard ways.
We are not trying to impress anyone with git wizardry. We are simply trying to provide the basics with a little understanding sprinkled in.

As a side note: we will be assuming that you are using git from the terminal. There is a GUI available from GitHub and it works super well for most cases; however, it's also self-explanatory in most cases.
Put another way: if you can understand the ways of the terminal, the git GUI will be much more straightforward. On the other hand, learning the GUI will not necessarily help you when using the terminal.

So, first things first. Make sure git is installed on your system and set it up with the following commands

```
git config --global user.name name
git config --global user.email name@email.com
```

Obviously, use your own name and e-mail... unless your name is actually *name* and your e-mail is actually *name@email.com*, in which case the above commands are correct.
In the rare case that a user named "name" with the e-mail "name@email.com" is reading this, I apologize for spoiling your anonymity.
For everyone else, remember that git is meant to facilitate collaborative code development, so we need to know who is submitting code so we can communicate more effectively later.
That said, it is alright to use a username and e-mail address that does not spoil your identity in the real world, so long as you are reachable by the information provided.

### Finding some code

Now we need to find a repository of code to work on. If you are starting your own repository or want to work on an internal network, this will not be too big of an issue.
If you just want to get the feel for how git works, I suggest going to [github.com](https://github.com/) and checking out the code developed there.
Note that you will not be able to contribute to any old directory on GitHub, simply because if anyone could contribute any code they wanted to any repository they wanted, the world would become incredibly chaotic.
Because of this, you may want to create a repository under your own GitHub username or make your own copy of someone elses code on GitHub by clicking the *fork* button:

<p>
    <img  class="center" src="res/fork.png" />
</p>

Note that if you have a fork of a particular code repository, you can ask the owner of the original code repository to pull your changes into their version of the code with a *pull request*, but we are getting ahead of ourselves here.
If you cannot think of what repository to work on and want to collaborate on this project in the future, feel free to fork the [Algorithm Archive](https://github.com/algorithm-archivists/algorithm-archive) and modify that!

Regardless, as long as there is a repository under your username on GitHub, we can continue by linking that remote GitHub location to your local git directory. First, we need to find the URL of the GitHub repository, as shown here:

<p>
    <img  class="center" src="res/clone.png" />
</p>

Note that there are 2 provided URLs here, one for *ssh* and another for *https*. From the user's perspective, the difference between the two is minimal: ssh requires the user to type only a password when interacting with the remote GitHub repository, while https requires both a username and password.
Now, you will probably be interacting with GitHub a lot, so ssh will definitely save time and is preferred for many people who use git a lot; however, [there is some initial set-up](https://help.github.com/articles/connecting-to-github-with-ssh/).
If you want, we can discuss the set-up in more detail later (just let me know!), but for now, we'll stick with https because it's more familiar to new users.

Once you have the URL, there are 2 ways to proceed:

**The easy way:**
```
git clone https://github.com/algorithm-archivists/algorithm-archive
```

**The not-so-easy way:**
```
mkdir algorithm_archive
cd algorithm_archive
git init
git remote add origin https://github.com/algorithm-archivists/algorithm-archive
git fetch
git merge origin/master
```

Here, `git clone` does every step of the *not-so-easy* way in one command, so the two methods are completely identical. Because of this, in most cases, I just use `git clone`; however, the *not-so-easy* way is much more explicit and helps us understand what is going on a little better.
For now, we will briefly describe each of the commands; however, we will definitely be covering them in more depth through this tutorial.
So, here it is, step-by-step:
1. `mkdir algorithm_archive`: make a directory. We can call this directory anything, but we'll call it algorithm_archive for now.
2. `git init`: initialize git
3. `git remote add origin https://github.com/algorithm-archivists/algorithm-archive`: add a remote location (the GitHub URL we found just a second ago). Again, we can call this remote location anything, but `git clone` always calls it `origin`, so well stick with that.
4. `git fetch`: update the local directory with the information from the remote online repository
5. `git merge origin/master`: merge those updates. Right now, the `origin/master` part of this command might seem like a bit of black octocat magic, but we will cover it in just a bit!

No matter how you initialize your git repository, your local directory will be linked with a remote location. If you ever want to see this location, simply type:

```
git remote -v
origin	https://github.com/user/algorithm-archive.git (fetch)
origin	https://github.com/user/algorithm-archive.git (push)
```

This provides information on different `remotes`. We'll talk about `fetch` and `push` a bit later.
Now, you might be asking yourself: If I am only connected to the URL I forked earlier, what happens when the owner of the main code repository pushes changes? How will I update my code when this happens?
Actually, you probably were not asking that question. It's not an obvious question to ask at all, but it's a useful question to move this tutorial forward.
The solution is simple: Add another `remote` like so:

```
git remote add upstream https://github.com/algorithm-archivists/algorithm-archive
```

Obviously, you can call the remote anything. I kinda arbitrarily chose to call it `upstream`. By adding this in, you can easily interact with the same codebase from multiple remote locations.
That said, we need to talk about how to do that.

### Committing to git

Now you have the repository linked to another online source. Note that you are not authorized to push changes onto the `upstream` URL, but that's alright for now. Let's just stick to modifying `origin`.
At this point, we can make any modification we want! I might suggest doing something simple:

```
echo name >> CONTRIBUTORS.md
```

Nothing crazy, just something so we can get the feeling of git. To see what files have been changed, type:

```
git status
```

This will show that `CONTRIBUTORS.md` has been modified.
If we want to save our changes, we need to add all of the files with changes to them to a package called a `commit`.
To add the files, simply type:

```
git add CONTRIBUTORS.md
```

Then if we type `git status` again, it will show that the file `CONTRIBUTORS.md` is in a *staging area* awaiting commit.
This simply means that git is waiting to make sure there are no other changes we want to package up.
Now we create the `commit` by typing

```
git commit -m "Adding name to contributors file"
```

Note that if you do not use the `-m` message flag (just `git commit`), git will open your default editor (probably vi) to ask for a message.
*Every git commit needs a git message!* Make the messages count.
Be as descriptive as possible!
If you want to see all commits that have ever been made on this repository, simply type

```
git log
```

This will show you the history so far. As a side note, it also shows why good, clean commit messages are essential to managing large, open-source projects.
If there are hundreds (or thousands) of commits, and one of the features implemented somewhere down the line has a bug, clean commit messages allow us to find when that feature was implemented and possibly when the bug arose.

Now let's say you want to checkout what the code looked like at a particular commit. To do this, we need to look at the generated unique string (SHA-1 checksum) associated with the commit we want and paste the first few (roughly 5) characters into the following command:

```
git checkout CHARS
```

It's incredibly unlikely that any two commits will share the first *n* characters, so this is unique enough for git to identify which commit we were referring to and send us back there, but here's where the notation gets a little crazy!
See, when we are sent back in time to the chosen commit (with the above command), we will be in a *detached head* state.
This refers to the term we use to describe the very latest commit, **HEAD**.
If we wanted to checkout the previous commit (for example), we would use `git checkout HEAD~1`, the second-to-last commit would be `HEAD~2`, and so on and so-forth.

When we checkout another commit, we are rolling the head of our commitment snake back to what it was in the past.
In the detached head state, we shouldn't really do any development. It's more of a read-only type of thing; however, if we want to develop the code starting at that commit, we could use

```
git checkout -b CHARS
```

But this requires a little explanation!


### Checkout these branches!

Now let's take a step to the side and talk about another fantastic git feature, *branches*.
At this point, we might have code forked under our own username on GitHub. This means that there could be at least 2 functioning versions of the code we are working on: our own fork and the original owner's fork.
That said, within each fork, there is the ability to have multiple lines of development, each one on a different *branch*.

If you are new to software development, this might not seem too useful; however, imagine you are working on a large, open-source project that thousands of people use.
At some point, you might want to re-organize a bunch of features in the code.
As a developer, you might not be sure whether all the features you are re-organizing will still work properly after re-organizing them, but you know the code needs to be modified!
The problem is that you have users who may need the features you might accidentally break.
For this reason, you might want to have a "master" branch -- one that is always working for the users, and a "development" branch -- one that is in the middle of creating new features for users.
In truth, there are dozens of reasons why developers might want to work on slightly different versions of the code.
Rather than spending time outlining all the potential reasons, let's just dive into how branches are made and maintained.

To check which branch you are on, simply type:

```
git branch
```

This will show you your currently active branch. If you haven't switched branches yet, you will probably by on `master`.
To switch branches, use

```
git checkout branch
```

And this will change all of the files on your local directory to match the branch you have swapped to.
Note that if you have local changes that will be overwritten when changing branches, git will note these changes and tell you to do something about them before switching to a new branch.
If you want to get rid of the changes, you could delete any files that are causing conflicts; however, this is barbaric and should be avoided in civilized society.
Another solution is to use a feature of git called the `stash`.
In many ways, this is much easier to do than deleting files manually. All you need to do is type:

```
git stash
```

This will stash all the local changes and bring the directory back to the latest HEAD. If you want to get your changes back, just use

```
git stash apply
```

Now, here's the problem: because `git stash` is so convenient, I tend to have the habit of stashing local changes quite often. This means that I have multiple modifications stored, all connected to different commits.
Quite frankly, it's a mess. That said, I can list out everything in my stash with

```
git stash list
```

and apply whichever stash I want with

```
git apply stash@{i}
```

Where `i` is the value of the stash item I want to apply.

Now, to be clear: I am not encouraging anyone to use `git stash` to hide away local changes and make branch traversal easier; however, if you are about to delete files, maybe try `git stash` instead?

Finally, we need to talk about a super sticky part of git: *merging branches*.
Following from the story above, you might have code in a development branch.
When you are happy with the changes in the development branch, you might want to merge those changes back to the master branch.
Assuming that no one was developing on the master branch and that the development branch is ahead of the master branch, this can be done with the following:

```
git checkout master
git merge branch
```

This is the simplest case, but it's rarely this simple. Often times, there will be development on different branches and when we merge these brances together, there will be conflicts.
These conflicts are noted in each of the files that need to be modified like so

```
I am writing about
<<<<<<< HEAD
things
=======
stuff
>>>>>>> development
```

Here, we wrote the phrase: "I am writing about *things*" on the master branch and "I am writing about *stuff*" on the development branch. Git got confused and let us know it has no idea what's going on.
To solve this, we will need to manually go through and find all the conflicts noted in the `git status` command and fix them to what they should be.
The easiest way to do this (in my opinion) can be found here: [https://help.github.com/articles/resolving-a-merge-conflict-using-the-command-line/](https://help.github.com/articles/resolving-a-merge-conflict-using-the-command-line/).

Note that there are a lot of good tools for this and everyone has their favorite choice.
I don't expect for too many users to run into merge conflicts while working with the Algorithm Archive, so I will omit much more discussion here, but let me know if you think I should cover this in more detail.
It's an incredibly difficult aspect of using git and will drive you nuts the first tie you see it, but after that, it will be much more straightforward.
Also, let me know if there's any tools you like, and I'll add them to this guide here.

### Interacting with GitHub

To this point, we have introduced the concept of `remote`s and how to set them up, but we have not discussed how to interact with them.
For the most part, there are only a few commands to keep in mind. The easiest one to explain is

```
git push
```

After we have made a commit (discussed above), we can push it to github like so

```
git push remote branch
```

For example, if you are pushing the `master` branch to the `origin` remote, it would be

```
git push origin master
```

Now, I personally like being explicit about which branch and remote we are working with, but you can tell git to ignore the `remote` and `branch` specifications by setting an upstream URL, which means running

```
git push -u remote branch
```

Once this is run, the remote and branch will be stored for later and you won't need to think about it ever again! (Well, you might need to think about it when working on more complicated things later)

Now, if `push`ing moves changes from your own computer to a repository online, it would make sense that `pull`ing does the opposite and moves changes from an online repository to your machine. Like before, this is straightforward:

```
git pull remote branch
```

However, there's a little more to it than that. In essence, `git pull` is running two separate commands. One updates your git repository with the information found on your remotes. This one is called `git fetch`.
The other one finds the changes and merges those changes with the branch found on your local machine. This is called `git merge` (as discussed before). When put together, it might look like:

```
git fetch
git merge origin/master
```

For now, I think that's all you will need: `git pull` and `git push`.
Now let's talk about something that will certainly happen in your programming career: mistakes.

### Dealing with mistakes

I cannot help with programming mistakes (typos and such), but when it comes to version control there are two times during which mistakes can be made: **before a commit** and **after a commit**.
Each of these have a different solution and have different repercussions depending on how you want to proceed with code development.
Note that these solutions can be quite complicated and may easily move beyond the scope of this text.
Because of this, I will link to appropriate documentation as necessary.

Firstly, let's talk about what happens when you make a mistake while your code is in the staging area, awaiting a commit.
Here, the solution is simple:

```
git reset
```

That's it. Don't overcomplicate it. You haven't committed to the code yet, so just unstage everything back to the `HEAD`.
The problem is that this command is quite nuanced and has plenty of other uses. This goes beyond the scope of this text, but you can find more information here: [https://git-scm.com/blog](https://git-scm.com/blog).

Now, what if your mistake was found after committing? Well, that's a little more complicated. Your mistake is already in your `git log`.
The easiest way to deal with this is to live with the mistake and make a new commit that fixes it later.
One way to reverse the commit completely is with

```
git revert commit
```

Where `commit` is whatever commit you want to undo from your `git log`.
Assuming you are working with a small team and don't mind having a somewhat dirty commit history where your mistakes haunt you forever in your `git log`, this is fine; however, if you want to remove the commit completely, you might need to think about using another command:

```
git rebase
```

The problem is that `git rebase` is complicated and could potentially destroy your codebase if it's used inappropriately.
Because of this, I often just live with my mistakes; however, in rare cases, having a clean `git log` is incredibly important.
I am not a git magician (yet), so I will not delve into what is essentially black magic to me. Instead, I'll link a guide: [https://git-scm.com/book/en/v2/Git-Branching-Rebasing](https://git-scm.com/book/en/v2/Git-Branching-Rebasing).

I know that this section is a little sparse and there's a lot I missed.
If you want to provide more information, feel free to do so and submit it via pull request.

### Concepts we missed

Unfortunately, this discussion has a scope. It is not meant to give you a deep, meaningful understanding of git.
Instead, we focused on the basics, with the hope of encouraging our community to start collaborating together.
The more you use git, the easier it will be to use in the future and the more it will start to make sense.
That said, due to the nature of this guide, there were a few things we missed, the two most important of which are **rebasing** and **merge conflicts**.

In addition, I need to be honest in saying that I am not the most qualified person to teach anyone how to use git or version control and that there are plenty of good guides out there already, so if you have any guides that you like, please let me know and I can add them to the end of this guide for more information.
