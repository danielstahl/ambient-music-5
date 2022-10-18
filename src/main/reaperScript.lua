
function CreateFolder(index, name)
  reaper.InsertTrackAtIndex(index, false)
  folder = reaper.GetTrack(0, index)
  reaper.GetSetMediaTrackInfo_String(folder, 'P_NAME', name, true)
  reaper.SetMediaTrackInfo_Value( folder, 'I_FOLDERDEPTH',1)
  reaper.SetMediaTrackInfo_Value(folder, 'I_FOLDERCOMPACT', 0)
end

function ImportAudio(index, channel, trackName, fileName, lastInFolder)
  local folderDepth = 0
  if lastInFolder then folderDepth = -1 end

  reaper.SetEditCurPos(0, false, false)
  reaper.InsertTrackAtIndex(index, false)
  tr = reaper.GetTrack(0, index)
  reaper.GetSetMediaTrackInfo_String(tr, 'P_NAME', trackName, true)
  reaper.SetMediaTrackInfo_Value( tr, 'I_FOLDERDEPTH',folderDepth)
  reaper.SetOnlyTrackSelected(tr, true)
  reaper.InsertMedia(fileName, 0)
  item = reaper.GetTrackMediaItem(tr, 0)
  take = reaper.GetActiveTake(item)
  reaper.SetMediaItemTakeInfo_Value(take, "I_CHANMODE", channel + 64 + 2)
end

audioFile = "/Users/danielstahl/Documents/Music/Pieces/Ambient Music/Ambient Music 5/stage/ambientMusic5Score.caf"

CreateFolder(0, "Pad")
ImportAudio(1, 1, "Pad Sub", audioFile, false)
ImportAudio(2, 3, "Pad Low", audioFile, false)
ImportAudio(3, 5, "Pad Middle", audioFile, false)
ImportAudio(4, 7, "Pad Middle High", audioFile, false)
ImportAudio(5, 9, "Pad High", audioFile, true)

CreateFolder(6, "Second theme")
ImportAudio(7, 11, "Second Sub", audioFile, false)
ImportAudio(8, 13, "Second Low", audioFile, false)
ImportAudio(9, 15, "Second Middle", audioFile, false)
ImportAudio(10, 17, "Second Middle High", audioFile, false)
ImportAudio(11, 19, "Second High", audioFile, true)
