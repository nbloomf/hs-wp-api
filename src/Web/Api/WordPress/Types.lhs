---
title: WordPress Types
---

> module Web.Api.WordPress.Types where

> data Context
>   = ViewContext
>   | EditContext
>   | EmbedContext

> data DateTime = DateTime

> data Guid = Guid

> data Uri = Uri

> data Slug = Slug

> data Status
>   = Publish
>   | Future
>   | Draft
>   | Pending
>   | Private

> type PostType = String

> data Password = Password

> data Title = Title

> data PostContent = PostContent

> type AuthorId = Integer

> data Excerpt = Excerpt

> type MediaId = Integer

> data OpenClosed
>   = Open
>   | Closed

> data PostFormat
>   = Standard
>   | Aside
>   | Chat
>   | Gallery
>   | Link
>   | Image
>   | Quote
>   | Status
>   | Video
>   | Audio

> data MetaFields = MetaFields

> data Category = Category
> data Tag = Tag

> -- | See https://developer.wordpress.org/rest-api/reference/posts/#schema.
> data Post = Post
>   { _postDate :: ApiValue DateTime
>   , _postDateGmt :: ApiValue DateTime
>   , _postGuid :: ApiValue Guid
>   , _postId :: ApiValue Integer
>   , _postLink :: ApiValue Uri
>   , _postModified :: ApiValue DateTime
>   , _postModifiedGmt :: ApiValue DateTime
>   , _postSlug :: ApiValue Slug
>   , _postStatus :: ApiValue Status
>   , _postType :: ApiValue PostType
>   , _postPassword :: ApiValue Password
>   , _postTitle :: ApiValue Title
>   , _postContent :: ApiValue PostContent
>   , _postAuthor :: ApiValue AuthorId
>   , _postExcerpt :: ApiValue Excerpt
>   , _postFeaturedMedia :: ApiValue MediaId
>   , _postCommentStatus :: ApiValue OpenClosed
>   , _postPingStatus :: ApiValue OpenClosed
>   , _postFormat :: ApiValue PostFormat
>   , _postMeta :: ApiValue MetaFields
>   , _postSticky :: ApiValue Bool
>   , _postTemplate :: ApiValue String
>   , _postCategories :: ApiValue [Category]
>   , _postTags :: ApiValue [Tag]
>   }

> data ApiValue a
>   = Value a
>   | NotInContext Context
>   | NotAvailable
