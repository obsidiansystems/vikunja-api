{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-unused-imports -fno-warn-unused-matches #-}

module Instances where

import Vikunja.Model
import Vikunja.Core

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V
import Data.String (fromString)

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck

import ApproxEq

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

#if MIN_VERSION_aeson(2,0,0)
#else
-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = arbitraryValue
#endif

arbitraryValue :: Gen A.Value
arbitraryValue =
  frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (fromString k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays

-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

arbitraryReduced :: Arbitrary a => Int -> Gen a
arbitraryReduced n = resize (n `div` 2) arbitrary

arbitraryReducedMaybe :: Arbitrary a => Int -> Gen (Maybe a)
arbitraryReducedMaybe 0 = elements [Nothing]
arbitraryReducedMaybe n = arbitraryReduced n

arbitraryReducedMaybeValue :: Int -> Gen (Maybe A.Value)
arbitraryReducedMaybeValue 0 = elements [Nothing]
arbitraryReducedMaybeValue n = do
  generated <- arbitraryReduced n
  if generated == Just A.Null
    then return Nothing
    else return generated

-- * Models

instance Arbitrary AuthToken where
  arbitrary = sized genAuthToken

genAuthToken :: Int -> Gen AuthToken
genAuthToken n =
  AuthToken
    <$> arbitraryReducedMaybe n -- authTokenToken :: Maybe Text
  
instance Arbitrary BackgroundImage where
  arbitrary = sized genBackgroundImage

genBackgroundImage :: Int -> Gen BackgroundImage
genBackgroundImage n =
  BackgroundImage
    <$> arbitraryReducedMaybe n -- backgroundImageBlurHash :: Maybe Text
    <*> arbitraryReducedMaybe n -- backgroundImageId :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- backgroundImageInfo :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- backgroundImageThumb :: Maybe Text
    <*> arbitraryReducedMaybe n -- backgroundImageUrl :: Maybe Text
  
instance Arbitrary FilesFile where
  arbitrary = sized genFilesFile

genFilesFile :: Int -> Gen FilesFile
genFilesFile n =
  FilesFile
    <$> arbitraryReducedMaybe n -- filesFileCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- filesFileId :: Maybe Int
    <*> arbitraryReducedMaybe n -- filesFileMime :: Maybe Text
    <*> arbitraryReducedMaybe n -- filesFileName :: Maybe Text
    <*> arbitraryReducedMaybe n -- filesFileSize :: Maybe Int
  
instance Arbitrary HandlerAuthURL where
  arbitrary = sized genHandlerAuthURL

genHandlerAuthURL :: Int -> Gen HandlerAuthURL
genHandlerAuthURL n =
  HandlerAuthURL
    <$> arbitraryReducedMaybe n -- handlerAuthURLUrl :: Maybe Text
  
instance Arbitrary MicrosofttodoMigration where
  arbitrary = sized genMicrosofttodoMigration

genMicrosofttodoMigration :: Int -> Gen MicrosofttodoMigration
genMicrosofttodoMigration n =
  MicrosofttodoMigration
    <$> arbitraryReducedMaybe n -- microsofttodoMigrationCode :: Maybe Text
  
instance Arbitrary MigrationStatus where
  arbitrary = sized genMigrationStatus

genMigrationStatus :: Int -> Gen MigrationStatus
genMigrationStatus n =
  MigrationStatus
    <$> arbitraryReducedMaybe n -- migrationStatusFinishedAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- migrationStatusId :: Maybe Int
    <*> arbitraryReducedMaybe n -- migrationStatusMigratorName :: Maybe Text
    <*> arbitraryReducedMaybe n -- migrationStatusStartedAt :: Maybe Text
  
instance Arbitrary ModelsAPIToken where
  arbitrary = sized genModelsAPIToken

genModelsAPIToken :: Int -> Gen ModelsAPIToken
genModelsAPIToken n =
  ModelsAPIToken
    <$> arbitraryReducedMaybe n -- modelsAPITokenCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsAPITokenExpiresAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsAPITokenId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsAPITokenPermissions :: Maybe (Map.Map String [Text])
    <*> arbitraryReducedMaybe n -- modelsAPITokenTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsAPITokenToken :: Maybe Text
  
instance Arbitrary ModelsBucket where
  arbitrary = sized genModelsBucket

genModelsBucket :: Int -> Gen ModelsBucket
genModelsBucket n =
  ModelsBucket
    <$> arbitraryReducedMaybe n -- modelsBucketCount :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBucketCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBucketCreatedBy :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsBucketId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBucketLimit :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBucketPosition :: Maybe Double
    <*> arbitraryReducedMaybe n -- modelsBucketProjectViewId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBucketTasks :: Maybe [ModelsTask]
    <*> arbitraryReducedMaybe n -- modelsBucketTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBucketUpdated :: Maybe Text
  
instance Arbitrary ModelsBulkAssignees where
  arbitrary = sized genModelsBulkAssignees

genModelsBulkAssignees :: Int -> Gen ModelsBulkAssignees
genModelsBulkAssignees n =
  ModelsBulkAssignees
    <$> arbitraryReducedMaybe n -- modelsBulkAssigneesAssignees :: Maybe [UserUser]
  
instance Arbitrary ModelsBulkTask where
  arbitrary = sized genModelsBulkTask

genModelsBulkTask :: Int -> Gen ModelsBulkTask
genModelsBulkTask n =
  ModelsBulkTask
    <$> arbitraryReducedMaybe n -- modelsBulkTaskAssignees :: Maybe [UserUser]
    <*> arbitraryReducedMaybe n -- modelsBulkTaskAttachments :: Maybe [ModelsTaskAttachment]
    <*> arbitraryReducedMaybe n -- modelsBulkTaskBucketId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBulkTaskCoverImageAttachmentId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBulkTaskCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBulkTaskCreatedBy :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsBulkTaskDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBulkTaskDone :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsBulkTaskDoneAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBulkTaskDueDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBulkTaskEndDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBulkTaskHexColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBulkTaskId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBulkTaskIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBulkTaskIndex :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBulkTaskIsFavorite :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsBulkTaskLabels :: Maybe [ModelsLabel]
    <*> arbitraryReducedMaybe n -- modelsBulkTaskPercentDone :: Maybe Double
    <*> arbitraryReducedMaybe n -- modelsBulkTaskPosition :: Maybe Double
    <*> arbitraryReducedMaybe n -- modelsBulkTaskPriority :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBulkTaskProjectId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBulkTaskReactions :: Maybe (Map.Map String [UserUser])
    <*> arbitraryReducedMaybe n -- modelsBulkTaskRelatedTasks :: Maybe (Map.Map String [ModelsTask])
    <*> arbitraryReducedMaybe n -- modelsBulkTaskReminders :: Maybe [ModelsTaskReminder]
    <*> arbitraryReducedMaybe n -- modelsBulkTaskRepeatAfter :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsBulkTaskRepeatMode :: Maybe ModelsTaskRepeatMode
    <*> arbitraryReducedMaybe n -- modelsBulkTaskStartDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBulkTaskSubscription :: Maybe ModelsSubscription
    <*> arbitraryReducedMaybe n -- modelsBulkTaskTaskIds :: Maybe [Int]
    <*> arbitraryReducedMaybe n -- modelsBulkTaskTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsBulkTaskUpdated :: Maybe Text
  
instance Arbitrary ModelsDatabaseNotifications where
  arbitrary = sized genModelsDatabaseNotifications

genModelsDatabaseNotifications :: Int -> Gen ModelsDatabaseNotifications
genModelsDatabaseNotifications n =
  ModelsDatabaseNotifications
    <$> arbitraryReducedMaybe n -- modelsDatabaseNotificationsCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsDatabaseNotificationsId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsDatabaseNotificationsName :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- modelsDatabaseNotificationsNotification :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- modelsDatabaseNotificationsRead :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsDatabaseNotificationsReadAt :: Maybe Text
  
instance Arbitrary ModelsLabel where
  arbitrary = sized genModelsLabel

genModelsLabel :: Int -> Gen ModelsLabel
genModelsLabel n =
  ModelsLabel
    <$> arbitraryReducedMaybe n -- modelsLabelCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsLabelCreatedBy :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsLabelDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsLabelHexColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsLabelId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsLabelTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsLabelUpdated :: Maybe Text
  
instance Arbitrary ModelsLabelTask where
  arbitrary = sized genModelsLabelTask

genModelsLabelTask :: Int -> Gen ModelsLabelTask
genModelsLabelTask n =
  ModelsLabelTask
    <$> arbitraryReducedMaybe n -- modelsLabelTaskCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsLabelTaskLabelId :: Maybe Int
  
instance Arbitrary ModelsLabelTaskBulk where
  arbitrary = sized genModelsLabelTaskBulk

genModelsLabelTaskBulk :: Int -> Gen ModelsLabelTaskBulk
genModelsLabelTaskBulk n =
  ModelsLabelTaskBulk
    <$> arbitraryReducedMaybe n -- modelsLabelTaskBulkLabels :: Maybe [ModelsLabel]
  
instance Arbitrary ModelsLinkSharing where
  arbitrary = sized genModelsLinkSharing

genModelsLinkSharing :: Int -> Gen ModelsLinkSharing
genModelsLinkSharing n =
  ModelsLinkSharing
    <$> arbitraryReducedMaybe n -- modelsLinkSharingCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsLinkSharingHash :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsLinkSharingId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsLinkSharingName :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsLinkSharingPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsLinkSharingRight :: Maybe ModelsRight
    <*> arbitraryReducedMaybe n -- modelsLinkSharingSharedBy :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsLinkSharingSharingType :: Maybe ModelsSharingType
    <*> arbitraryReducedMaybe n -- modelsLinkSharingUpdated :: Maybe Text
  
instance Arbitrary ModelsMessage where
  arbitrary = sized genModelsMessage

genModelsMessage :: Int -> Gen ModelsMessage
genModelsMessage n =
  ModelsMessage
    <$> arbitraryReducedMaybe n -- modelsMessageMessage :: Maybe Text
  
instance Arbitrary ModelsProject where
  arbitrary = sized genModelsProject

genModelsProject :: Int -> Gen ModelsProject
genModelsProject n =
  ModelsProject
    <$> arbitraryReducedMaybe n -- modelsProjectBackgroundBlurHash :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- modelsProjectBackgroundInformation :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- modelsProjectCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectHexColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsProjectIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectIsArchived :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsProjectIsFavorite :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsProjectOwner :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsProjectParentProjectId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsProjectPosition :: Maybe Double
    <*> arbitraryReducedMaybe n -- modelsProjectSubscription :: Maybe ModelsSubscription
    <*> arbitraryReducedMaybe n -- modelsProjectTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectUpdated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectViews :: Maybe [ModelsProjectView]
  
instance Arbitrary ModelsProjectDuplicate where
  arbitrary = sized genModelsProjectDuplicate

genModelsProjectDuplicate :: Int -> Gen ModelsProjectDuplicate
genModelsProjectDuplicate n =
  ModelsProjectDuplicate
    <$> arbitraryReducedMaybe n -- modelsProjectDuplicateDuplicatedProject :: Maybe ModelsProject
    <*> arbitraryReducedMaybe n -- modelsProjectDuplicateParentProjectId :: Maybe Int
  
instance Arbitrary ModelsProjectUser where
  arbitrary = sized genModelsProjectUser

genModelsProjectUser :: Int -> Gen ModelsProjectUser
genModelsProjectUser n =
  ModelsProjectUser
    <$> arbitraryReducedMaybe n -- modelsProjectUserCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectUserId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsProjectUserRight :: Maybe ModelsRight
    <*> arbitraryReducedMaybe n -- modelsProjectUserUpdated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectUserUserId :: Maybe Text
  
instance Arbitrary ModelsProjectView where
  arbitrary = sized genModelsProjectView

genModelsProjectView :: Int -> Gen ModelsProjectView
genModelsProjectView n =
  ModelsProjectView
    <$> arbitraryReducedMaybe n -- modelsProjectViewBucketConfiguration :: Maybe [ModelsProjectViewBucketConfiguration]
    <*> arbitraryReducedMaybe n -- modelsProjectViewBucketConfigurationMode :: Maybe ModelsBucketConfigurationModeKind
    <*> arbitraryReducedMaybe n -- modelsProjectViewCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectViewDefaultBucketId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsProjectViewDoneBucketId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsProjectViewFilter :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectViewId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsProjectViewPosition :: Maybe Double
    <*> arbitraryReducedMaybe n -- modelsProjectViewProjectId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsProjectViewTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectViewUpdated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectViewViewKind :: Maybe ModelsProjectViewKind
  
instance Arbitrary ModelsProjectViewBucketConfiguration where
  arbitrary = sized genModelsProjectViewBucketConfiguration

genModelsProjectViewBucketConfiguration :: Int -> Gen ModelsProjectViewBucketConfiguration
genModelsProjectViewBucketConfiguration n =
  ModelsProjectViewBucketConfiguration
    <$> arbitraryReducedMaybe n -- modelsProjectViewBucketConfigurationFilter :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsProjectViewBucketConfigurationTitle :: Maybe Text
  
instance Arbitrary ModelsReaction where
  arbitrary = sized genModelsReaction

genModelsReaction :: Int -> Gen ModelsReaction
genModelsReaction n =
  ModelsReaction
    <$> arbitraryReducedMaybe n -- modelsReactionCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsReactionUser :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsReactionValue :: Maybe Text
  
instance Arbitrary ModelsRouteDetail where
  arbitrary = sized genModelsRouteDetail

genModelsRouteDetail :: Int -> Gen ModelsRouteDetail
genModelsRouteDetail n =
  ModelsRouteDetail
    <$> arbitraryReducedMaybe n -- modelsRouteDetailMethod :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsRouteDetailPath :: Maybe Text
  
instance Arbitrary ModelsSavedFilter where
  arbitrary = sized genModelsSavedFilter

genModelsSavedFilter :: Int -> Gen ModelsSavedFilter
genModelsSavedFilter n =
  ModelsSavedFilter
    <$> arbitraryReducedMaybe n -- modelsSavedFilterCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsSavedFilterDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsSavedFilterFilters :: Maybe ModelsTaskCollection
    <*> arbitraryReducedMaybe n -- modelsSavedFilterId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsSavedFilterIsFavorite :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsSavedFilterOwner :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsSavedFilterTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsSavedFilterUpdated :: Maybe Text
  
instance Arbitrary ModelsSubscription where
  arbitrary = sized genModelsSubscription

genModelsSubscription :: Int -> Gen ModelsSubscription
genModelsSubscription n =
  ModelsSubscription
    <$> arbitraryReducedMaybe n -- modelsSubscriptionCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsSubscriptionEntity :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsSubscriptionEntityId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsSubscriptionId :: Maybe Int
  
instance Arbitrary ModelsTask where
  arbitrary = sized genModelsTask

genModelsTask :: Int -> Gen ModelsTask
genModelsTask n =
  ModelsTask
    <$> arbitraryReducedMaybe n -- modelsTaskAssignees :: Maybe [UserUser]
    <*> arbitraryReducedMaybe n -- modelsTaskAttachments :: Maybe [ModelsTaskAttachment]
    <*> arbitraryReducedMaybe n -- modelsTaskBucketId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskCoverImageAttachmentId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskCreatedBy :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsTaskDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskDone :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsTaskDoneAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskDueDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskEndDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskHexColor :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskIdentifier :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskIndex :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskIsFavorite :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsTaskLabels :: Maybe [ModelsLabel]
    <*> arbitraryReducedMaybe n -- modelsTaskPercentDone :: Maybe Double
    <*> arbitraryReducedMaybe n -- modelsTaskPosition :: Maybe Double
    <*> arbitraryReducedMaybe n -- modelsTaskPriority :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskProjectId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskReactions :: Maybe (Map.Map String [UserUser])
    <*> arbitraryReducedMaybe n -- modelsTaskRelatedTasks :: Maybe (Map.Map String [ModelsTask])
    <*> arbitraryReducedMaybe n -- modelsTaskReminders :: Maybe [ModelsTaskReminder]
    <*> arbitraryReducedMaybe n -- modelsTaskRepeatAfter :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskRepeatMode :: Maybe ModelsTaskRepeatMode
    <*> arbitraryReducedMaybe n -- modelsTaskStartDate :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskSubscription :: Maybe ModelsSubscription
    <*> arbitraryReducedMaybe n -- modelsTaskTitle :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskUpdated :: Maybe Text
  
instance Arbitrary ModelsTaskAssginee where
  arbitrary = sized genModelsTaskAssginee

genModelsTaskAssginee :: Int -> Gen ModelsTaskAssginee
genModelsTaskAssginee n =
  ModelsTaskAssginee
    <$> arbitraryReducedMaybe n -- modelsTaskAssgineeCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskAssgineeUserId :: Maybe Int
  
instance Arbitrary ModelsTaskAttachment where
  arbitrary = sized genModelsTaskAttachment

genModelsTaskAttachment :: Int -> Gen ModelsTaskAttachment
genModelsTaskAttachment n =
  ModelsTaskAttachment
    <$> arbitraryReducedMaybe n -- modelsTaskAttachmentCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskAttachmentCreatedBy :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsTaskAttachmentFile :: Maybe FilesFile
    <*> arbitraryReducedMaybe n -- modelsTaskAttachmentId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskAttachmentTaskId :: Maybe Int
  
instance Arbitrary ModelsTaskBucket where
  arbitrary = sized genModelsTaskBucket

genModelsTaskBucket :: Int -> Gen ModelsTaskBucket
genModelsTaskBucket n =
  ModelsTaskBucket
    <$> arbitraryReducedMaybe n -- modelsTaskBucketBucketId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskBucketProjectViewId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskBucketTaskDone :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsTaskBucketTaskId :: Maybe Int
  
instance Arbitrary ModelsTaskCollection where
  arbitrary = sized genModelsTaskCollection

genModelsTaskCollection :: Int -> Gen ModelsTaskCollection
genModelsTaskCollection n =
  ModelsTaskCollection
    <$> arbitraryReducedMaybe n -- modelsTaskCollectionFilter :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskCollectionFilterIncludeNulls :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsTaskCollectionOrderBy :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- modelsTaskCollectionSortBy :: Maybe [Text]
  
instance Arbitrary ModelsTaskComment where
  arbitrary = sized genModelsTaskComment

genModelsTaskComment :: Int -> Gen ModelsTaskComment
genModelsTaskComment n =
  ModelsTaskComment
    <$> arbitraryReducedMaybe n -- modelsTaskCommentAuthor :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsTaskCommentComment :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskCommentCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskCommentId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskCommentReactions :: Maybe (Map.Map String [UserUser])
    <*> arbitraryReducedMaybe n -- modelsTaskCommentUpdated :: Maybe Text
  
instance Arbitrary ModelsTaskPosition where
  arbitrary = sized genModelsTaskPosition

genModelsTaskPosition :: Int -> Gen ModelsTaskPosition
genModelsTaskPosition n =
  ModelsTaskPosition
    <$> arbitraryReducedMaybe n -- modelsTaskPositionPosition :: Maybe Double
    <*> arbitraryReducedMaybe n -- modelsTaskPositionProjectViewId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskPositionTaskId :: Maybe Int
  
instance Arbitrary ModelsTaskRelation where
  arbitrary = sized genModelsTaskRelation

genModelsTaskRelation :: Int -> Gen ModelsTaskRelation
genModelsTaskRelation n =
  ModelsTaskRelation
    <$> arbitraryReducedMaybe n -- modelsTaskRelationCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTaskRelationCreatedBy :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsTaskRelationOtherTaskId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskRelationRelationKind :: Maybe ModelsRelationKind
    <*> arbitraryReducedMaybe n -- modelsTaskRelationTaskId :: Maybe Int
  
instance Arbitrary ModelsTaskReminder where
  arbitrary = sized genModelsTaskReminder

genModelsTaskReminder :: Int -> Gen ModelsTaskReminder
genModelsTaskReminder n =
  ModelsTaskReminder
    <$> arbitraryReducedMaybe n -- modelsTaskReminderRelativePeriod :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTaskReminderRelativeTo :: Maybe ModelsReminderRelation
    <*> arbitraryReducedMaybe n -- modelsTaskReminderReminder :: Maybe Text
  
instance Arbitrary ModelsTeam where
  arbitrary = sized genModelsTeam

genModelsTeam :: Int -> Gen ModelsTeam
genModelsTeam n =
  ModelsTeam
    <$> arbitraryReducedMaybe n -- modelsTeamCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamCreatedBy :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsTeamDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTeamIncludePublic :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsTeamIsPublic :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsTeamMembers :: Maybe [ModelsTeamUser]
    <*> arbitraryReducedMaybe n -- modelsTeamName :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamOidcId :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamUpdated :: Maybe Text
  
instance Arbitrary ModelsTeamMember where
  arbitrary = sized genModelsTeamMember

genModelsTeamMember :: Int -> Gen ModelsTeamMember
genModelsTeamMember n =
  ModelsTeamMember
    <$> arbitraryReducedMaybe n -- modelsTeamMemberAdmin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsTeamMemberCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamMemberId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTeamMemberUsername :: Maybe Text
  
instance Arbitrary ModelsTeamProject where
  arbitrary = sized genModelsTeamProject

genModelsTeamProject :: Int -> Gen ModelsTeamProject
genModelsTeamProject n =
  ModelsTeamProject
    <$> arbitraryReducedMaybe n -- modelsTeamProjectCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamProjectId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTeamProjectRight :: Maybe ModelsRight
    <*> arbitraryReducedMaybe n -- modelsTeamProjectTeamId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTeamProjectUpdated :: Maybe Text
  
instance Arbitrary ModelsTeamUser where
  arbitrary = sized genModelsTeamUser

genModelsTeamUser :: Int -> Gen ModelsTeamUser
genModelsTeamUser n =
  ModelsTeamUser
    <$> arbitraryReducedMaybe n -- modelsTeamUserAdmin :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsTeamUserCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamUserEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamUserId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTeamUserName :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamUserUpdated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamUserUsername :: Maybe Text
  
instance Arbitrary ModelsTeamWithRight where
  arbitrary = sized genModelsTeamWithRight

genModelsTeamWithRight :: Int -> Gen ModelsTeamWithRight
genModelsTeamWithRight n =
  ModelsTeamWithRight
    <$> arbitraryReducedMaybe n -- modelsTeamWithRightCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamWithRightCreatedBy :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsTeamWithRightDescription :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamWithRightId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsTeamWithRightIncludePublic :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsTeamWithRightIsPublic :: Maybe Bool
    <*> arbitraryReducedMaybe n -- modelsTeamWithRightMembers :: Maybe [ModelsTeamUser]
    <*> arbitraryReducedMaybe n -- modelsTeamWithRightName :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamWithRightOidcId :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsTeamWithRightRight :: Maybe ModelsRight
    <*> arbitraryReducedMaybe n -- modelsTeamWithRightUpdated :: Maybe Text
  
instance Arbitrary ModelsUserWithRight where
  arbitrary = sized genModelsUserWithRight

genModelsUserWithRight :: Int -> Gen ModelsUserWithRight
genModelsUserWithRight n =
  ModelsUserWithRight
    <$> arbitraryReducedMaybe n -- modelsUserWithRightCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsUserWithRightEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsUserWithRightId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsUserWithRightName :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsUserWithRightRight :: Maybe ModelsRight
    <*> arbitraryReducedMaybe n -- modelsUserWithRightUpdated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsUserWithRightUsername :: Maybe Text
  
instance Arbitrary ModelsWebhook where
  arbitrary = sized genModelsWebhook

genModelsWebhook :: Int -> Gen ModelsWebhook
genModelsWebhook n =
  ModelsWebhook
    <$> arbitraryReducedMaybe n -- modelsWebhookCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsWebhookCreatedBy :: Maybe UserUser
    <*> arbitraryReducedMaybe n -- modelsWebhookEvents :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- modelsWebhookId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsWebhookProjectId :: Maybe Int
    <*> arbitraryReducedMaybe n -- modelsWebhookSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsWebhookTargetUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- modelsWebhookUpdated :: Maybe Text
  
instance Arbitrary NotificationsDatabaseNotification where
  arbitrary = sized genNotificationsDatabaseNotification

genNotificationsDatabaseNotification :: Int -> Gen NotificationsDatabaseNotification
genNotificationsDatabaseNotification n =
  NotificationsDatabaseNotification
    <$> arbitraryReducedMaybe n -- notificationsDatabaseNotificationCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- notificationsDatabaseNotificationId :: Maybe Int
    <*> arbitraryReducedMaybe n -- notificationsDatabaseNotificationName :: Maybe Text
    <*> arbitraryReducedMaybeValue n -- notificationsDatabaseNotificationNotification :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- notificationsDatabaseNotificationReadAt :: Maybe Text
  
instance Arbitrary OpenidCallback where
  arbitrary = sized genOpenidCallback

genOpenidCallback :: Int -> Gen OpenidCallback
genOpenidCallback n =
  OpenidCallback
    <$> arbitraryReducedMaybe n -- openidCallbackCode :: Maybe Text
    <*> arbitraryReducedMaybe n -- openidCallbackRedirectUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- openidCallbackScope :: Maybe Text
  
instance Arbitrary OpenidProvider where
  arbitrary = sized genOpenidProvider

genOpenidProvider :: Int -> Gen OpenidProvider
genOpenidProvider n =
  OpenidProvider
    <$> arbitraryReducedMaybe n -- openidProviderAuthUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- openidProviderClientId :: Maybe Text
    <*> arbitraryReducedMaybe n -- openidProviderKey :: Maybe Text
    <*> arbitraryReducedMaybe n -- openidProviderLogoutUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- openidProviderName :: Maybe Text
    <*> arbitraryReducedMaybe n -- openidProviderScope :: Maybe Text
  
instance Arbitrary TodoistMigration where
  arbitrary = sized genTodoistMigration

genTodoistMigration :: Int -> Gen TodoistMigration
genTodoistMigration n =
  TodoistMigration
    <$> arbitraryReducedMaybe n -- todoistMigrationCode :: Maybe Text
  
instance Arbitrary TrelloMigration where
  arbitrary = sized genTrelloMigration

genTrelloMigration :: Int -> Gen TrelloMigration
genTrelloMigration n =
  TrelloMigration
    <$> arbitraryReducedMaybe n -- trelloMigrationCode :: Maybe Text
  
instance Arbitrary UserAPIUserPassword where
  arbitrary = sized genUserAPIUserPassword

genUserAPIUserPassword :: Int -> Gen UserAPIUserPassword
genUserAPIUserPassword n =
  UserAPIUserPassword
    <$> arbitraryReducedMaybe n -- userAPIUserPasswordEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userAPIUserPasswordId :: Maybe Int
    <*> arbitraryReducedMaybe n -- userAPIUserPasswordPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- userAPIUserPasswordUsername :: Maybe Text
  
instance Arbitrary UserEmailConfirm where
  arbitrary = sized genUserEmailConfirm

genUserEmailConfirm :: Int -> Gen UserEmailConfirm
genUserEmailConfirm n =
  UserEmailConfirm
    <$> arbitraryReducedMaybe n -- userEmailConfirmToken :: Maybe Text
  
instance Arbitrary UserEmailUpdate where
  arbitrary = sized genUserEmailUpdate

genUserEmailUpdate :: Int -> Gen UserEmailUpdate
genUserEmailUpdate n =
  UserEmailUpdate
    <$> arbitraryReducedMaybe n -- userEmailUpdateNewEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userEmailUpdatePassword :: Maybe Text
  
instance Arbitrary UserLogin where
  arbitrary = sized genUserLogin

genUserLogin :: Int -> Gen UserLogin
genUserLogin n =
  UserLogin
    <$> arbitraryReducedMaybe n -- userLoginLongToken :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userLoginPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLoginTotpPasscode :: Maybe Text
    <*> arbitraryReducedMaybe n -- userLoginUsername :: Maybe Text
  
instance Arbitrary UserPasswordReset where
  arbitrary = sized genUserPasswordReset

genUserPasswordReset :: Int -> Gen UserPasswordReset
genUserPasswordReset n =
  UserPasswordReset
    <$> arbitraryReducedMaybe n -- userPasswordResetNewPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- userPasswordResetToken :: Maybe Text
  
instance Arbitrary UserPasswordTokenRequest where
  arbitrary = sized genUserPasswordTokenRequest

genUserPasswordTokenRequest :: Int -> Gen UserPasswordTokenRequest
genUserPasswordTokenRequest n =
  UserPasswordTokenRequest
    <$> arbitraryReducedMaybe n -- userPasswordTokenRequestEmail :: Maybe Text
  
instance Arbitrary UserTOTP where
  arbitrary = sized genUserTOTP

genUserTOTP :: Int -> Gen UserTOTP
genUserTOTP n =
  UserTOTP
    <$> arbitraryReducedMaybe n -- userTOTPEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- userTOTPSecret :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTOTPUrl :: Maybe Text
  
instance Arbitrary UserTOTPPasscode where
  arbitrary = sized genUserTOTPPasscode

genUserTOTPPasscode :: Int -> Gen UserTOTPPasscode
genUserTOTPPasscode n =
  UserTOTPPasscode
    <$> arbitraryReducedMaybe n -- userTOTPPasscodePasscode :: Maybe Text
  
instance Arbitrary UserToken where
  arbitrary = sized genUserToken

genUserToken :: Int -> Gen UserToken
genUserToken n =
  UserToken
    <$> arbitraryReducedMaybe n -- userTokenCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- userTokenId :: Maybe Int
    <*> arbitraryReducedMaybe n -- userTokenToken :: Maybe Text
  
instance Arbitrary UserUser where
  arbitrary = sized genUserUser

genUserUser :: Int -> Gen UserUser
genUserUser n =
  UserUser
    <$> arbitraryReducedMaybe n -- userUserCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- userUserEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- userUserId :: Maybe Int
    <*> arbitraryReducedMaybe n -- userUserName :: Maybe Text
    <*> arbitraryReducedMaybe n -- userUserUpdated :: Maybe Text
    <*> arbitraryReducedMaybe n -- userUserUsername :: Maybe Text
  
instance Arbitrary V1AuthInfo where
  arbitrary = sized genV1AuthInfo

genV1AuthInfo :: Int -> Gen V1AuthInfo
genV1AuthInfo n =
  V1AuthInfo
    <$> arbitraryReducedMaybe n -- v1AuthInfoLocal :: Maybe V1LocalAuthInfo
    <*> arbitraryReducedMaybe n -- v1AuthInfoOpenidConnect :: Maybe V1OpenIDAuthInfo
  
instance Arbitrary V1LegalInfo where
  arbitrary = sized genV1LegalInfo

genV1LegalInfo :: Int -> Gen V1LegalInfo
genV1LegalInfo n =
  V1LegalInfo
    <$> arbitraryReducedMaybe n -- v1LegalInfoImprintUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1LegalInfoPrivacyPolicyUrl :: Maybe Text
  
instance Arbitrary V1LinkShareAuth where
  arbitrary = sized genV1LinkShareAuth

genV1LinkShareAuth :: Int -> Gen V1LinkShareAuth
genV1LinkShareAuth n =
  V1LinkShareAuth
    <$> arbitraryReducedMaybe n -- v1LinkShareAuthPassword :: Maybe Text
  
instance Arbitrary V1LocalAuthInfo where
  arbitrary = sized genV1LocalAuthInfo

genV1LocalAuthInfo :: Int -> Gen V1LocalAuthInfo
genV1LocalAuthInfo n =
  V1LocalAuthInfo
    <$> arbitraryReducedMaybe n -- v1LocalAuthInfoEnabled :: Maybe Bool
  
instance Arbitrary V1OpenIDAuthInfo where
  arbitrary = sized genV1OpenIDAuthInfo

genV1OpenIDAuthInfo :: Int -> Gen V1OpenIDAuthInfo
genV1OpenIDAuthInfo n =
  V1OpenIDAuthInfo
    <$> arbitraryReducedMaybe n -- v1OpenIDAuthInfoEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1OpenIDAuthInfoProviders :: Maybe [OpenidProvider]
  
instance Arbitrary V1UserAvatarProvider where
  arbitrary = sized genV1UserAvatarProvider

genV1UserAvatarProvider :: Int -> Gen V1UserAvatarProvider
genV1UserAvatarProvider n =
  V1UserAvatarProvider
    <$> arbitraryReducedMaybe n -- v1UserAvatarProviderAvatarProvider :: Maybe Text
  
instance Arbitrary V1UserDeletionRequestConfirm where
  arbitrary = sized genV1UserDeletionRequestConfirm

genV1UserDeletionRequestConfirm :: Int -> Gen V1UserDeletionRequestConfirm
genV1UserDeletionRequestConfirm n =
  V1UserDeletionRequestConfirm
    <$> arbitraryReducedMaybe n -- v1UserDeletionRequestConfirmToken :: Maybe Text
  
instance Arbitrary V1UserPassword where
  arbitrary = sized genV1UserPassword

genV1UserPassword :: Int -> Gen V1UserPassword
genV1UserPassword n =
  V1UserPassword
    <$> arbitraryReducedMaybe n -- v1UserPasswordNewPassword :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserPasswordOldPassword :: Maybe Text
  
instance Arbitrary V1UserPasswordConfirmation where
  arbitrary = sized genV1UserPasswordConfirmation

genV1UserPasswordConfirmation :: Int -> Gen V1UserPasswordConfirmation
genV1UserPasswordConfirmation n =
  V1UserPasswordConfirmation
    <$> arbitraryReducedMaybe n -- v1UserPasswordConfirmationPassword :: Maybe Text
  
instance Arbitrary V1UserSettings where
  arbitrary = sized genV1UserSettings

genV1UserSettings :: Int -> Gen V1UserSettings
genV1UserSettings n =
  V1UserSettings
    <$> arbitraryReducedMaybe n -- v1UserSettingsDefaultProjectId :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1UserSettingsDiscoverableByEmail :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1UserSettingsDiscoverableByName :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1UserSettingsEmailRemindersEnabled :: Maybe Bool
    <*> arbitraryReducedMaybeValue n -- v1UserSettingsFrontendSettings :: Maybe A.Value
    <*> arbitraryReducedMaybe n -- v1UserSettingsLanguage :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserSettingsName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserSettingsOverdueTasksRemindersEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1UserSettingsOverdueTasksRemindersTime :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserSettingsTimezone :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserSettingsWeekStart :: Maybe Int
  
instance Arbitrary V1UserWithSettings where
  arbitrary = sized genV1UserWithSettings

genV1UserWithSettings :: Int -> Gen V1UserWithSettings
genV1UserWithSettings n =
  V1UserWithSettings
    <$> arbitraryReducedMaybe n -- v1UserWithSettingsCreated :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserWithSettingsDeletionScheduledAt :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserWithSettingsEmail :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserWithSettingsId :: Maybe Int
    <*> arbitraryReducedMaybe n -- v1UserWithSettingsIsLocalUser :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1UserWithSettingsName :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserWithSettingsSettings :: Maybe V1UserSettings
    <*> arbitraryReducedMaybe n -- v1UserWithSettingsUpdated :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1UserWithSettingsUsername :: Maybe Text
  
instance Arbitrary V1VikunjaInfos where
  arbitrary = sized genV1VikunjaInfos

genV1VikunjaInfos :: Int -> Gen V1VikunjaInfos
genV1VikunjaInfos n =
  V1VikunjaInfos
    <$> arbitraryReducedMaybe n -- v1VikunjaInfosAuth :: Maybe V1AuthInfo
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosAvailableMigrators :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosCaldavEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosDemoModeEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosEmailRemindersEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosEnabledBackgroundProviders :: Maybe [Text]
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosFrontendUrl :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosLegal :: Maybe V1LegalInfo
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosLinkSharingEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosMaxFileSize :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosMotd :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosPublicTeamsEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosRegistrationEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosTaskAttachmentsEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosTaskCommentsEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosTotpEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosUserDeletionEnabled :: Maybe Bool
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosVersion :: Maybe Text
    <*> arbitraryReducedMaybe n -- v1VikunjaInfosWebhooksEnabled :: Maybe Bool
  
instance Arbitrary WebHTTPError where
  arbitrary = sized genWebHTTPError

genWebHTTPError :: Int -> Gen WebHTTPError
genWebHTTPError n =
  WebHTTPError
    <$> arbitraryReducedMaybe n -- webHTTPErrorCode :: Maybe Int
    <*> arbitraryReducedMaybe n -- webHTTPErrorMessage :: Maybe Text
  



instance Arbitrary ModelsBucketConfigurationModeKind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ModelsProjectViewKind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ModelsRelationKind where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ModelsReminderRelation where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ModelsRight where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ModelsSharingType where
  arbitrary = arbitraryBoundedEnum

instance Arbitrary ModelsTaskRepeatMode where
  arbitrary = arbitraryBoundedEnum

