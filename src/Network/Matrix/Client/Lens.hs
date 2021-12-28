module Network.Matrix.Client.Lens where

import qualified Data.Aeson as J
import Control.Lens
import Data.Coerce
import qualified Data.Text as T
import qualified Data.Map.Strict as M
import Network.Matrix.Client

_mtBody :: Lens' MessageText T.Text
_mtBody = lens getter setter
  where
    getter = mtBody
    setter mt t = mt { mtBody = t }

_mtType :: Lens' MessageText MessageTextType
_mtType = lens getter setter
  where
    getter = mtType
    setter mt t = mt { mtType = t }

_mtFormat :: Lens' MessageText (Maybe T.Text)
_mtFormat = lens getter setter
  where
    getter = mtFormat
    setter mt t = mt { mtFormat = t }

_mtFormattedBody :: Lens' MessageText (Maybe T.Text)
_mtFormattedBody = lens getter setter
  where
    getter = mtFormattedBody
    setter mt t = mt { mtFormattedBody = t}

_RoomMessageText :: Lens' RoomMessage MessageText
_RoomMessageText = lens getter setter
  where
    getter = coerce
    setter _ t = RoomMessageText t

_EventRoomMessage :: Prism' Event RoomMessage
_EventRoomMessage = prism' to' from'
  where
    to' = EventRoomMessage
    from' (EventRoomMessage msg) = Just msg
    from' _ = Nothing

_EventRoomReply :: Prism' Event (EventID, RoomMessage)
_EventRoomReply = prism' to' from'
  where
    to' (eid, rm) = EventRoomReply eid rm
    from' (EventRoomReply eid rm) = Just (eid, rm)
    from' _ = Nothing

_EventRoomEdit :: Prism' Event ((EventID, RoomMessage), RoomMessage)
_EventRoomEdit = prism' to' from'
  where
    to' (oldEvent, newMsg) = EventRoomEdit oldEvent newMsg
    from' (EventRoomEdit oldEvent newMsg) = Just (oldEvent, newMsg)
    from' _ = Nothing

_EventUnknown :: Prism' Event J.Object
_EventUnknown = prism' to' from'
  where
    to' = EventUnknown
    from' (EventUnknown obj) = Just obj
    from' _ = Nothing

_efLimit :: Lens' EventFilter (Maybe Int)
_efLimit = lens getter setter
  where
    getter = efLimit
    setter ef lim =  ef { efLimit = lim }

_efNotSenders :: Lens' EventFilter (Maybe [T.Text])
_efNotSenders = lens getter setter
  where
    getter = efNotSenders
    setter ef ns = ef { efNotSenders = ns }

_efNotTypes :: Lens' EventFilter (Maybe [T.Text])
_efNotTypes = lens getter setter
  where
    getter = efNotTypes
    setter ef nt = ef { efNotTypes = nt }

_efSenders :: Lens' EventFilter (Maybe [T.Text])
_efSenders = lens getter setter
  where
    getter = efSenders
    setter ef s = ef { efSenders = s }

_efTypes :: Lens' EventFilter (Maybe [T.Text])
_efTypes = lens getter setter
  where
    getter = efTypes
    setter ef t = ef { efTypes = t }

_refLimit :: Lens' RoomEventFilter (Maybe Int)
_refLimit = lens getter setter
  where
    getter = refLimit
    setter ref rl = ref { refLimit = rl }

_refNotSenders :: Lens' RoomEventFilter (Maybe [T.Text])
_refNotSenders = lens getter setter
  where
    getter = refNotSenders
    setter ref ns = ref { refNotSenders = ns }

_refNotTypes :: Lens' RoomEventFilter (Maybe [T.Text])
_refNotTypes = lens getter setter
  where
    getter = refNotTypes
    setter ref rnt = ref { refNotTypes = rnt }

_refSenders :: Lens' RoomEventFilter (Maybe [T.Text])
_refSenders = lens getter setter
 where
   getter = refSenders
   setter ref rs = ref { refSenders = rs }

_refTypes :: Lens' RoomEventFilter (Maybe [T.Text])
_refTypes = lens getter setter
  where
    getter = refTypes
    setter ref rt = ref { refTypes = rt }

_refLazyLoadMembers :: Lens' RoomEventFilter (Maybe Bool)
_refLazyLoadMembers = lens getter setter
  where
    getter = refLazyLoadMembers
    setter ref rldm = ref { refLazyLoadMembers = rldm }

_refIncludeRedundantMembers :: Lens' RoomEventFilter (Maybe Bool)
_refIncludeRedundantMembers = lens getter setter
  where
    getter = refIncludeRedundantMembers
    setter ref rirm = ref { refIncludeRedundantMembers = rirm }

_refNotRooms :: Lens' RoomEventFilter (Maybe [T.Text])
_refNotRooms = lens getter setter
  where
    getter = refNotRooms
    setter ref rnr = ref { refNotRooms = rnr }

_refRooms :: Lens' RoomEventFilter (Maybe [T.Text])
_refRooms = lens getter setter
  where
    getter = refRooms
    setter ref rr = ref { refRooms = rr }

_refContainsUrl :: Lens' RoomEventFilter (Maybe Bool)
_refContainsUrl = lens getter setter
  where
    getter = refContainsUrl
    setter ref rcu = ref { refContainsUrl = rcu }

_sfLimit :: Lens' StateFilter (Maybe Int)
_sfLimit = lens getter setter
  where
    getter = sfLimit
    setter sf sfl = sf { sfLimit = sfl }

_sfNotSenders :: Lens' StateFilter (Maybe [T.Text])
_sfNotSenders = lens getter setter
  where
    getter = sfNotSenders
    setter sf sfns = sf { sfNotSenders = sfns}

_sfTypes :: Lens' StateFilter (Maybe [T.Text])
_sfTypes = lens getter setter
  where
    getter = sfTypes
    setter sf sft = sf { sfTypes = sft }

_sfLazyLoadMembers :: Lens' StateFilter (Maybe Bool)
_sfLazyLoadMembers = lens getter setter
  where
    getter = sfLazyLoadMembers
    setter sf sflm = sf { sfLazyLoadMembers = sflm }

_sfIncludeRedundantMembers :: Lens' StateFilter (Maybe Bool)
_sfIncludeRedundantMembers = lens getter setter
  where
    getter = sfIncludeRedundantMembers
    setter sf sfirm = sf { sfIncludeRedundantMembers = sfirm }

_sfNotRooms :: Lens' StateFilter (Maybe [T.Text])
_sfNotRooms = lens getter setter
  where
    getter = sfNotRooms
    setter sf sfnr = sf { sfNotRooms = sfnr }

_sfRooms :: Lens' StateFilter (Maybe [T.Text])
_sfRooms = lens getter setter
  where
    getter = sfRooms
    setter sf sfr = sf { sfRooms = sfr }

_sfContainsUrl :: Lens' StateFilter (Maybe Bool)
_sfContainsUrl = lens getter setter
  where
    getter = sfContains_url
    setter sf cu = sf { sfContains_url = cu }

_rfNotRooms :: Lens' RoomFilter (Maybe [T.Text])
_rfNotRooms = lens getter setter
  where
    getter = rfNotRooms
    setter rm rfnr = rm { rfNotRooms = rfnr }

_rfRooms :: Lens' RoomFilter (Maybe [T.Text])
_rfRooms = lens getter setter
  where
    getter = rfRooms
    setter rm rfr = rm { rfRooms = rfr }

_rfEphemeral :: Lens' RoomFilter (Maybe RoomEventFilter)
_rfEphemeral = lens getter setter
  where
    getter = rfEphemeral
    setter rm rfe = rm { rfEphemeral = rfe }

_rfIncludeLeave :: Lens' RoomFilter (Maybe Bool)
_rfIncludeLeave = lens getter setter
  where
    getter = rfIncludeLeave
    setter rm rfil = rm { rfIncludeLeave = rfil }

_rfState :: Lens' RoomFilter (Maybe StateFilter)
_rfState = lens getter setter
  where
    getter = rfState
    setter rm rfs = rm { rfState = rfs }

_rfTimeline :: Lens' RoomFilter (Maybe RoomEventFilter)
_rfTimeline = lens getter setter
  where
    getter = rfTimeline
    setter rm rft = rm { rfTimeline = rft }

_rfAccountData :: Lens' RoomFilter (Maybe RoomEventFilter)
_rfAccountData = lens getter setter
  where
    getter = rfAccountData
    setter rm rfad = rm { rfAccountData = rfad }

_filterEventFields :: Lens' Filter (Maybe [T.Text])
_filterEventFields = lens getter setter
  where
    getter = filterEventFields
    setter fltr fef = fltr { filterEventFields = fef }

_filterEventFormat :: Lens' Filter (Maybe EventFormat)
_filterEventFormat = lens getter setter
  where
    getter = filterEventFormat
    setter fltr fef = fltr { filterEventFormat = fef }

_filterPresence :: Lens' Filter (Maybe EventFilter)
_filterPresence = lens getter setter
  where
    getter = filterPresence
    setter fltr fp = fltr { filterPresence = fp }

_filterAccountData :: Lens' Filter (Maybe EventFilter)
_filterAccountData = lens getter setter
  where
    getter = filterAccountData
    setter fltr fac = fltr { filterAccountData = fac }

_filterRoom :: Lens' Filter (Maybe RoomFilter)
_filterRoom = lens getter setter
  where
    getter = filterRoom
    setter fltr fr = fltr { filterRoom = fr }

_reContent :: Lens' RoomEvent Event
_reContent = lens getter setter
  where
    getter = reContent
    setter rEvent rc = rEvent { reContent = rc }

_reType :: Lens' RoomEvent T.Text
_reType = lens getter setter
  where
    getter = reType
    setter rEvent rt = rEvent { reType = rt }

_reEventId :: Lens' RoomEvent EventID
_reEventId = lens getter setter
  where
    getter = reEventId
    setter rEvent reid = rEvent { reEventId = reid }

_reSender :: Lens' RoomEvent Author
_reSender = lens getter setter
  where
    getter = reSender
    setter rEvent res = rEvent { reSender = res }

_rsJoinedMemberCount :: Lens' RoomSummary (Maybe Int)
_rsJoinedMemberCount = lens getter setter
  where
    getter = rsJoinedMemberCount
    setter rs rsjmc = rs { rsJoinedMemberCount = rsjmc }

_rsInvitedMemberCount :: Lens' RoomSummary (Maybe Int)
_rsInvitedMemberCount = lens getter setter
  where
    getter = rsInvitedMemberCount
    setter rs rsimc = rs { rsInvitedMemberCount = rsimc }

_tsEvents :: Lens' TimelineSync (Maybe [RoomEvent])
_tsEvents = lens getter setter
  where
    getter = tsEvents
    setter ts tse = ts { tsEvents = tse }

_tsLimited :: Lens' TimelineSync (Maybe Bool)
_tsLimited = lens getter setter
  where
    getter = tsLimited
    setter ts tsl = ts { tsLimited = tsl }

_tsPrevBatch :: Lens' TimelineSync (Maybe T.Text)
_tsPrevBatch = lens getter setter
  where
    getter = tsPrevBatch
    setter ts tspb = ts { tsPrevBatch = tspb }

_jrsSummary :: Lens' JoinedRoomSync (Maybe RoomSummary)
_jrsSummary = lens getter setter
  where
    getter = jrsSummary
    setter jrs jrss = jrs { jrsSummary = jrss }

_jrsTimeline :: Lens' JoinedRoomSync TimelineSync
_jrsTimeline = lens getter setter
  where
    getter = jrsTimeline
    setter jrs jrst = jrs { jrsTimeline = jrst }

_srNextBatch :: Lens' SyncResult T.Text
_srNextBatch = lens getter setter
  where
    getter = srNextBatch
    setter sr srnb = sr { srNextBatch = srnb }

_srRooms :: Lens' SyncResult (Maybe SyncResultRoom)
_srRooms = lens getter setter
  where
    getter = srRooms
    setter sr srr = sr { srRooms = srr }

_srrJoin :: Lens' SyncResultRoom (Maybe (M.Map T.Text JoinedRoomSync))
_srrJoin = lens getter setter
  where
    getter = srrJoin
    setter srr srrj = srr { srrJoin = srrj }

_srrInvite :: Lens' SyncResultRoom (Maybe (M.Map T.Text InvitedRoomSync))
_srrInvite = lens getter setter
  where
    getter = srrInvite
    setter srr srri = srr { srrInvite = srri }
