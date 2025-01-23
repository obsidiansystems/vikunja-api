{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PartialTypeSignatures #-}

module Main where

import Data.Typeable (Proxy(..))
import Test.Hspec
import Test.Hspec.QuickCheck

import PropMime
import Instances ()

import Vikunja.Model
import Vikunja.MimeTypes

main :: IO ()
main =
  hspec $ modifyMaxSize (const 10) $ do
    describe "JSON instances" $ do
      pure ()
      propMimeEq MimeJSON (Proxy :: Proxy AuthToken)
      propMimeEq MimeJSON (Proxy :: Proxy BackgroundImage)
      propMimeEq MimeJSON (Proxy :: Proxy FilesFile)
      propMimeEq MimeJSON (Proxy :: Proxy HandlerAuthURL)
      propMimeEq MimeJSON (Proxy :: Proxy MicrosofttodoMigration)
      propMimeEq MimeJSON (Proxy :: Proxy MigrationStatus)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsAPIToken)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsBucket)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsBucketConfigurationModeKind)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsBulkAssignees)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsBulkTask)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsDatabaseNotifications)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsLabel)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsLabelTask)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsLabelTaskBulk)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsLinkSharing)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsMessage)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsProject)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsProjectDuplicate)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsProjectUser)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsProjectView)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsProjectViewBucketConfiguration)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsProjectViewKind)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsReaction)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsRelationKind)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsReminderRelation)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsRight)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsRouteDetail)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsSavedFilter)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsSharingType)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsSubscription)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTask)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTaskAssginee)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTaskAttachment)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTaskBucket)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTaskCollection)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTaskComment)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTaskPosition)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTaskRelation)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTaskReminder)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTaskRepeatMode)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTeam)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTeamMember)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTeamProject)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTeamUser)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsTeamWithRight)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsUserWithRight)
      propMimeEq MimeJSON (Proxy :: Proxy ModelsWebhook)
      propMimeEq MimeJSON (Proxy :: Proxy NotificationsDatabaseNotification)
      propMimeEq MimeJSON (Proxy :: Proxy OpenidCallback)
      propMimeEq MimeJSON (Proxy :: Proxy OpenidProvider)
      propMimeEq MimeJSON (Proxy :: Proxy TodoistMigration)
      propMimeEq MimeJSON (Proxy :: Proxy TrelloMigration)
      propMimeEq MimeJSON (Proxy :: Proxy UserAPIUserPassword)
      propMimeEq MimeJSON (Proxy :: Proxy UserEmailConfirm)
      propMimeEq MimeJSON (Proxy :: Proxy UserEmailUpdate)
      propMimeEq MimeJSON (Proxy :: Proxy UserLogin)
      propMimeEq MimeJSON (Proxy :: Proxy UserPasswordReset)
      propMimeEq MimeJSON (Proxy :: Proxy UserPasswordTokenRequest)
      propMimeEq MimeJSON (Proxy :: Proxy UserTOTP)
      propMimeEq MimeJSON (Proxy :: Proxy UserTOTPPasscode)
      propMimeEq MimeJSON (Proxy :: Proxy UserToken)
      propMimeEq MimeJSON (Proxy :: Proxy UserUser)
      propMimeEq MimeJSON (Proxy :: Proxy V1AuthInfo)
      propMimeEq MimeJSON (Proxy :: Proxy V1LegalInfo)
      propMimeEq MimeJSON (Proxy :: Proxy V1LinkShareAuth)
      propMimeEq MimeJSON (Proxy :: Proxy V1LocalAuthInfo)
      propMimeEq MimeJSON (Proxy :: Proxy V1OpenIDAuthInfo)
      propMimeEq MimeJSON (Proxy :: Proxy V1UserAvatarProvider)
      propMimeEq MimeJSON (Proxy :: Proxy V1UserDeletionRequestConfirm)
      propMimeEq MimeJSON (Proxy :: Proxy V1UserPassword)
      propMimeEq MimeJSON (Proxy :: Proxy V1UserPasswordConfirmation)
      propMimeEq MimeJSON (Proxy :: Proxy V1UserSettings)
      propMimeEq MimeJSON (Proxy :: Proxy V1UserWithSettings)
      propMimeEq MimeJSON (Proxy :: Proxy V1VikunjaInfos)
      propMimeEq MimeJSON (Proxy :: Proxy WebHTTPError)
      
