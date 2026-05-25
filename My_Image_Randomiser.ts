//------------------------------------------------------------------------------
// My Image Randomiser
//
// This script will randomise the 16 scenes, with an even distribution of the 4 sub-options.
//
//------------------------------------------------------------------------------

// -----------------------------------------------------------------------------
// The import and export functions below come in pre-filled from the template.
import { registerSimple, registerEditor, component } from "@gorilla/compiled/task-builder.js";
import { BaseRandomiseSpreadsheet, BaseRandomiseSpreadsheetFactory } from "@gorilla/compiled/task-builder.js";

//------------------------------------------------------------------------------
export interface My_Image_RandomiserFactory extends BaseRandomiseSpreadsheetFactory {
  // NONE NEEDED
}

//------------------------------------------------------------------------------
@component("base.component.My_Image_Randomiser")
export class My_Image_Randomiser extends BaseRandomiseSpreadsheet<My_Image_RandomiserFactory> {

  /** 
   *  The Fisher-Yates algorithm guarantees a statistically uniform shuffle
   *  Every permutation is equally likely - which is why it's preferred over naive random sorting approaches.
   *  Math.random() is automatically seeded from an OS-level entropy source -> No way to replay a shuffle.
   */
  private shuffleDeckString(input: string): string {
    const chars = Array.from(input);

    for (let i = chars.length - 1; i > 0; i--) {
      const j = (Math.random() * (i + 1)) | 0;
      const temp = chars[i];
      chars[i] = chars[j];
      chars[j] = temp;
    }

    return chars.join("");
  }

  public randomiseSpreadsheet(name: string, columns: string[], rows: any[]) {
    let deck1 = "0123456789ABCDEF";
    let deck2 = "1234123412341234";

    const subSetName = ["original-img", "normal-low-n", "normal-noise", "oddity-low-n", "oddity-noise"];
    const subSetNoise = ["Error", "Low", "High", "Low", "High"];
    const subSetOddity = ["Error", "None", "None", "Present", "Present"];
    const subSetQuadrant = ["Error", "1", "2", "3", "4"];
    const imageSetName = {
      "0": "01", "1": "02", "2": "03", "3": "04",
      "4": "05", "5": "06", "6": "07", "7": "08",
      "8": "09", "9": "10", "A": "11", "B": "12",
      "C": "13", "D": "14", "E": "15", "F": "16",
    };

    deck1 = this.shuffleDeckString(deck1);
    deck2 = this.shuffleDeckString(deck2);

    const trialRowIndexes = rows.reduce((indexes: number[], row: any, index: number) => {
      if (row.display === "trial") {
        indexes.push(index);
      }
      return indexes;
    }, []);
    const rowCount = trialRowIndexes.length;

    const newRows: any[] = [...rows];

    for (let i = 0; i < rowCount; i++) {
      const imageSetKey = deck1[i];
      const subSetKey = deck2[i];
      const x = imageSetKey ? imageSetName[imageSetKey as keyof typeof imageSetName] : `${i}`;
      const subSetIndex = subSetKey ? Number(subSetKey) : 0;
      const y = subSetName[subSetIndex] ?? subSetName[0];

      newRows[trialRowIndexes[i]].env_image = `img-${x}-${y}.png`;
      newRows[trialRowIndexes[i]].image_set = x;
      newRows[trialRowIndexes[i]].noisiness = subSetNoise[subSetIndex];
      newRows[trialRowIndexes[i]].oddity = subSetOddity[subSetIndex];
      newRows[trialRowIndexes[i]].quadrant = subSetQuadrant[subSetIndex];
    }

    return newRows;
  }
}

//------------------------------------------------------------------------------
registerEditor("My_Image_Randomiser", {
  label: "My_Image_Randomiser",
  icon: "fas fa-dice",
  form: {
    elements: [],
  },
});

//------------------------------------------------------------------------------
registerSimple("taskSpreadsheetRandomisationComponent", "My_Image_Randomiser", {
  description: "Silvino Rodrigues oddity trial custom image randomiser",
});

//------------------------------------------------------------------------------
// oef
