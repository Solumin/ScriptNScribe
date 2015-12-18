# Script-n-Scribe UI Plan

The current state of the Script-n-Scribe UI is admittedly disappointing. This is
due to a lack of experience with both Threepenny-GUI and Web development on my
part, and a lack of time to learn. I hope that this document clarifies my ideas
about the UI and serve as a guide for future development.

## UI Layout

As mentioned in the paper, the basic layout of the Script-n-Scribe UI has a code
pane on the left, a results music pane on the right, and controls in the middle.

### Code Pane

Currently, the code pane is a simple textarea element. This works for very
incredibly basic text editing, but more advanced features like syntax
highlighting would be very helpful.

The plan is to integrate the [Ace][ace] editor, which is a fully-featured
embeddable editor. Secondary to this is creating a syntax highlighting file for
Breve, but considering the size of the language, this shouldn't be horrendously
difficult. Fully integrating the editor with the Breve UI would be ideal,
including letting users select their own color scheme and key bindings. (Ace
support Vim and emacs key bindings out of the box.)

### Music Pane

As a high-level concept, drawing a score should be a fairly simple task. Using
an HTML5 canvas, the clefs, note heads, stems and flags can all be loaded as
separate images and combined to create the notes as necessary. But engraving
music can be quite complex:

* There should be a certain number of notes per measure, adding up to the number
  of beats in the measure. This is determined by the time signature, which Breve
  does not implement.
* Notes smaller than the quarter note (eighth, sixteenth, etc.) are drawn in
  groups using a thick black line called a beam to join their stems.
* The beam needs to adjust to the height of the notes' stems, which may change
  suddenly as the orientation of the stem changes depending on the note.
* Notes are not always grouped with beams.
* Notes may have ties joining them. (Breve does not support ties at this time.)

And so on. Fortunately, there are Javascript libraries for engraving music. In
particular, the [Vexflow][vf] library produces high-quality scores.
Unfortunately the code required to create the scores is complex, and working
with it indirectly through Threepenny may be quite difficult.

Of course, implementing the bare minimum to display music is still an option.

Whichever method is chosen for drawing the scores, the question of user
interaction with the score is still wide open. A promising library for adding
interactivity to the HTML5 elements of the score is [Raphaël][raph]. Besides
offering quite powerful drag-and-drop and animation tools, Raphaël is also
mentioned in the VexFlow documentation, suggesting that integrating the two
libraries should be feasible.

### Additional Information

The code pane is more-or-less constant, as there's no reason for the user to
input any other kinds of text. However, the right side of the UI can hold
several information panes.

Breve programs produce three kinds of output that may be of interest to the
user:

* Traces. These can be helpful for debugging, or just for showing the user
  what's going on behind the scenes.
* Text output. Breve programs don't always produce music, and even when they do
  it's in the form of a Haskell object that can be shown. This pane shows the
  plain text output of the program.
* Music output, or the actual score. Only valid if the program outputs a music
  object. (This was covered in the previous section.)

The trace and text output panes can just be text areas, with no particular need
for syntax highlighting.

### Controls

- Choose Mode: Ultimately, Script-n-Scribe should be able to perform both ad hoc
  and live mode synthesis. The user should be able to choose between them.
  - Synchronize: If the user is in ad hoc mode, they need buttons for starting
    the synchronization process.
- Switch Results Pane: Given we make available all three panes in the Additional
  Information section, the user must be able to switch between them!
- Play: Perform the music.
- Editor controls: Switching the control scheme and color scheme, for example.

## Interacting with Breve and Synthesis

Currently, the UI does not show the user anything that is going on, and silently
fails if the parser, evaluator or synthesiser encounter any errors. There's not
really anything that can be done in the UI to fix this, since the fault lies
entirely with the other components.

But once proper error handling is implemented, the UI will be able to properly
alert the user of errors and hopefully hint at how to fix them.

* Syntax errors: Alert the user of the failed parse. If possible, highlight the
  offending code in the editor. (May be possible with Ace.)
* Type errors: It may not be possible to alert the user of the source code
  location, but perhaps we can point towards the snippet containing the error.
* Evaluation errors: Similar to type errors, we may be able to point out the
  general vicinity of the code.
* Synthesis errors: These should hopefully be rare, but we could prompt the user
  to try a different synthesis mode.

[ace]: https://ace.c9.io/#nav=about
[vf]: http://www.vexflow.com/
[raph]: http://raphaeljs.com/
