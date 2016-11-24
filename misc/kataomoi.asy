pen fillColor = heavygreen;

real heartWidth = 1inch;
real heartHeight = 1.75inch;

real keyholeWidth = 0.3inch;

real lockWidth = 0.3inch;
real lockHeight = 0.75inch;

real heartTopHalf = heartHeight - heartWidth/2;

path heart = (0,0)..controls (0.5*heartWidth,0.4*heartTopHalf) and (0, 0)..(0.5*heartWidth,0.4*heartTopHalf)::(heartWidth,heartTopHalf){up}::(0.5*heartWidth,heartHeight){left}::{down}(0,heartTopHalf){up}::(-0.5*heartWidth,heartHeight){left}::(-heartWidth,heartTopHalf){down}::(-0.5*heartWidth,0.4*heartTopHalf)..controls (0,0) and (-0.5*heartWidth,0.4*heartTopHalf)..cycle;

real keyholeY = 2/3*heartTopHalf;

path keyhole = arc((0,keyholeY+keyholeWidth/2), (keyholeWidth/4, keyholeY), (-keyholeWidth/4, keyholeY))--(-keyholeWidth/2,keyholeY-keyholeWidth)--(keyholeWidth/2,keyholeY-keyholeWidth)--cycle;

fill(heart^^keyhole, evenodd+fillColor);

real lockOuter = (heartWidth + lockWidth)/2;
real lockInner = (heartWidth - lockWidth)/2;
real lockTopHalf = heartHeight + lockHeight - heartWidth/2;
real lockTop = heartHeight + lockHeight;
real lockOuterTop = lockTop + lockWidth/2;
real lockInnerTop = lockTop - lockWidth/2;

path lock = ((lockOuter,heartHeight-0.1inch){up}::(lockOuter,lockTopHalf){up}::(0,lockOuterTop){left}::(-lockOuter,lockTopHalf){down}::{down}(-lockOuter,heartHeight-0.1inch)--(-lockInner,heartHeight-0.1inch){up}::(-lockInner,lockTopHalf){up}::(0,lockInnerTop){right}::(lockInner,lockTopHalf){down}::{down}(lockInner,heartHeight-0.1inch)--cycle);

fill(lock, fillColor);
