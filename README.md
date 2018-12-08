# Useful general info

## Homeworks

Homework handover is done through the Moodle system we have at FMI.
For questions on homeworks you can (ordered by preference):

1. Use the mailing list.
2. Contact me directly (through one of the methods).

Public methods are best, because others can also read discussions there.

Your points on each homework and total points will be shown in a google doc, which doesn't exist as of yet.


### [Moodle course for homework hand in]

### [Google doc for grades and comments on homeworks (hover over your points)]

### Homework guidelines

#### Function names
>    Functions must be named the same way as in the tasks.

#### File organisation
>    If you are handing in a single file, it must be named \<your fn\>.{rkt,scm,hs,..} and it must **export**/**provide** all the functions
>    which are specified in the tasks.
>
>    If you are handing in more than one file, they should be in a folder named \<your fn\>, which is afterwards **tar**/**zip**-ed.
>    Furthermore, inside it there should be one "all" file named \<your fn\>.{rkt,scm,..}, which (imports) **require**s all the other files,
>    and then (re-exports) **provide**s all the functions which are specified in the tasks.

>    Example:
>
>    We have two files for our homework `helpers.hs` and `actualhw01.hs` and `actualhw02.hs`.
>    We now create a folder `81248`. Inside it we place `helpers.hs`,
>    `actualhw01.hs` and `actualhw02.hs`. Inside it we also create a file `81248.hs`,
>    in which we import all the important files (`actualhw01.hs` and (`actualhw02.hs`) in this case
>    and export all the functions that are actually required for the homework. Afterwards we zip the whole folder `81248`
>    into the zip `81248.zip`. We now hand in this `81248.zip` file for grading.




## Grading?

| Type     | How many? | Points for each | Total |
| -------- |----------:| ---------------:|------:|
| Homework | 7         | 10              | 70    |
| Project  | 1         | 60              | 60    |

> But what about grades?

**>=110** points are a **6**

## Contact me:

* [Mail]
* [Messenger]

## GitHub repository

[Repo]

Homeworks should be under `hws`.

Notes should be under `notes`.

What we do live should be under `exercises`.

## Mailing list

[Mailing list]

You will receive homeworks and other info here.
It's also fine to have discussions if you want.

**You need to confirm your registration! It might be in Spam.**

## Pull requests

If you notice any mistakes feel free to open a pull request.


[Mail]: mailto:godzbanebane@gmail.com
[Messenger]: https://www.facebook.com/g.lyubenov78
[Repo]: https://github.com/googleson78/fp-pract1819
[Mailing list]: https://lists.uni-sofia.bg/cgi-bin/mailman/listinfo/fpract
[Moodle course for homework hand in]: https://learn.fmi.uni-sofia.bg/course/view.php?id=4681
[Google doc for grades and comments on homeworks (hover over your points)]: https://docs.google.com/spreadsheets/d/1fkqtbtaaxRHx1f26lZM8XrM8bHdh7MHcZoagP5vSYW8
