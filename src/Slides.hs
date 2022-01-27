module Slides where

import Control.Concurrent
import Data.Foldable
import Control.Lens
import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Reader
import Data.Bool
import Data.Coerce
import Data.List as L
import Data.Maybe
import Data.Text as T
import Debug.Trace
import HtmlT
import Text.Read
import qualified Data.Set as Set
import qualified JavaScript.Array as JS

import "this" Life
import "this" Utils

type SlideNum = Int

type SlideStep = Int

slidesWidget :: (SlideNum, SlideStep) -> Html ()
slidesWidget = \case
  (0, s) -> do
    div_ [class_ "Slide-1"] do
      h1_ "HtmlT — Lightweight Frontend Library for GHCJS"
      h2_ "Table of Contents"
      ul_ [class_ "Slide-1-contents"] do
        li_ $ a_ [class_ "no_visited", href_ "#1-0"] "Motivation for HtmlT"
        li_ $ a_ [class_ "no_visited", href_ "#2-0"] "Example of a Simple Widget"
        li_ $ a_ [class_ "no_visited", href_ "#3-0"] "What is HtmlT?"
        li_ $ a_ [class_ "no_visited", href_ "#4-0"] "Event, Dynamic and DynRef"
        li_ $ a_ [class_ "no_visited", href_ "#5-0"] "How to Become Dynamic?"
        li_ $ a_ [class_ "no_visited", href_ "#5-0"] "What is DynRef?"
        li_ $ a_ [class_ "no_visited", href_ "#6-0"] "What is Transact?"
        li_ $ a_ [class_ "no_visited", href_ "#7-0"] "Improvements compare to Reflex"
        li_ $ a_ [class_ "no_visited", href_ "#7-0"] "Game of Life"
        li_ $ a_ [class_ "no_visited", href_ "#8-0"] "Faster Game of Life"
      div_ [class_ "Slide-1-legend"] do
        shortcuts
          [ ("Step forward", ["→", "Space"])
          , ("Step backward", ["←", "Backspace"])
          , ("To the last slide", ["↑", "End"])
          , ("To the first slide", ["↓", "Home"])
          , ("Next slide", ["Ctrl + →", "Ctrl + Space"])
          , ("Previous slide", ["Ctrl + ←", "Ctrl + Backspace"])
          ]
      div_ [class_ "Slide-1-links"] do
        div_ "Vladislav Lagunov"
        div_ "January 2022"
        extLnk "https://github.com/lagunoff/htmlt"
        extLnk "https://github.com/lagunoff/htmlt-presentation"
        extLnk "https://lagunoff.github.io/htmlt-presentation"
        where
          extLnk l = div_ $ a_ [class_ "Slide-1-link no_visited", href_ l]
            (text l) >> unsafeHtml externalLinkIcon
  (1, s) -> do
    h1_ "Motivation for HtmlT"
    br_
    ul_ [class_ "BulletedList"] do
      when (s > 0) $ li_ "Fullstack development in Haskell"
      when (s > 1) $ li_ "Shortage of lightweight tools in the industry"
      when (s > 2) $ li_ "Having fun time working on an interesting project"
      when (s > 3) $ li_ "Getting good knowledge of the tools you are using"
  (2, s) -> do
    h1_ "Example of a Simple Widget"
    br_
    unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token hvariable\">main</span> <span class=\"token operator\">::</span> <span class=\"token constant\">IO</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span>\n\
\<span class=\"token hvariable\">main</span> <span class=\"token operator\">=</span> <span class=\"token hvariable\">void</span> <span class=\"token operator\">$</span> <span class=\"token hvariable\">attachToBody</span> <span class=\"token keyword\">do</span>\n\
\  <span class=\"token hvariable\">counterRef</span> <span class=\"token operator\">&lt;-</span> <span class=\"token hvariable\">newRef</span> <span class=\"token operator\">@</span><span class=\"token constant\">Int</span> <span class=\"token number\">0</span>\n\
\  <span class=\"token hvariable\">div_</span> <span class=\"token punctuation\">[</span><span class=\"token hvariable\">class_</span> <span class=\"token string\">\"wrapper\"</span><span class=\"token punctuation\">]</span> <span class=\"token keyword\">do</span>\n\
\    <span class=\"token hvariable\">input_</span> <span class=\"token punctuation\">[</span><span class=\"token hvariable\">type_</span> <span class=\"token string\">\"text\"</span><span class=\"token punctuation\">]</span> <span class=\"token keyword\">do</span>\n\
\      <span class=\"token hvariable\">toggleClass</span> <span class=\"token string\">\"border-red\"</span> <span class=\"token operator\">$</span> <span class=\"token builtin\">odd</span> <span class=\"token operator\">&lt;$&gt;</span> <span class=\"token hvariable\">fromRef</span> <span class=\"token hvariable\">counterRef</span>\n\
\      <span class=\"token hvariable\">dynProp</span> <span class=\"token string\">\"value\"</span> <span class=\"token operator\">$</span> <span class=\"token constant\">T</span><span class=\"token punctuation\">.</span><span class=\"token builtin\">pack</span> <span class=\"token operator\">.</span> <span class=\"token builtin\">show</span> <span class=\"token operator\">&lt;$&gt;</span> <span class=\"token hvariable\">fromRef</span> <span class=\"token hvariable\">counterRef</span>\n\
\      <span class=\"token hvariable\">onDecoder</span> <span class=\"token string\">\"input\"</span> <span class=\"token hvariable\">valueDecoder</span> <span class=\"token operator\">\\</span><span class=\"token hvariable\">val</span> <span class=\"token operator\">-&gt;</span>\n\
\        <span class=\"token hvariable\">traverse_</span> <span class=\"token punctuation\">(</span><span class=\"token hvariable\">writeRef</span> <span class=\"token hvariable\">counterRef</span><span class=\"token punctuation\">)</span> <span class=\"token operator\">.</span> <span class=\"token hvariable\">readMaybe</span> <span class=\"token operator\">.</span> <span class=\"token hvariable\">T<span class=\"token punctuation\">.</span>unpack</span> <span class=\"token operator\">$</span> <span class=\"token hvariable\">val</span>\n\
\    <span class=\"token hvariable\">button_</span> <span class=\"token punctuation\">[</span><span class=\"token hvariable\">title_</span> <span class=\"token string\">\"Click to decrease counter\"</span><span class=\"token punctuation\">]</span> <span class=\"token keyword\">do</span>\n\
\      <span class=\"token hvariable\">on_</span> <span class=\"token string\">\"click\"</span> <span class=\"token operator\">$</span> <span class=\"token hvariable\">modifyRef</span> <span class=\"token hvariable\">counterRef</span> <span class=\"token builtin\">pred</span>\n\
\      <span class=\"token hvariable\">text</span> <span class=\"token string\">\"Decrease\"</span>\n\
\    <span class=\"token hvariable\">button_</span> <span class=\"token punctuation\">[</span><span class=\"token hvariable\">title_</span> <span class=\"token string\">\"Click to increase counter\"</span><span class=\"token punctuation\">]</span> <span class=\"token keyword\">do</span>\n\
\      <span class=\"token hvariable\">on_</span> <span class=\"token string\">\"click\"</span> <span class=\"token operator\">$</span> <span class=\"token hvariable\">modifyRef</span> <span class=\"token hvariable\">counterRef</span> <span class=\"token builtin\">succ</span>\n\
\      <span class=\"token hvariable\">text</span> <span class=\"token string\">\"Increase\"</span></code></pre>"
    el "hr" blank
    h3_ "Result:"
    counterRef <- newRef @Int 0
    div_ [class_ "wrapper"] do
      input_ [type_ "text"] do
        toggleClass "border-red" $ odd <$> fromRef counterRef
        dynProp "value" $ T.pack . show <$> fromRef counterRef
        onDecoder "input" valueDecoder \val ->
          traverse_ (writeRef counterRef) . readMaybe . T.unpack $ val
      br_
      button_ [title_ "Click to decrease counter"] do
        on_ "click" $ modifyRef counterRef pred
        text "Decrease"
      button_ [title_ "Click to increase counter"] do
        on_ "click" $ modifyRef counterRef succ
        text "Increase"
  (3, s) -> do
    h1_ "HtmlT is a newtype over ReaderT"
    br_
    when (s > -1) do
      unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">newtype</span> <span class=\"token constant\">HtmlT</span> <span class=\"token hvariable\">m</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">=</span> <span class=\"token constant\">HtmlT</span> <span class=\"token punctuation\">{</span><span class=\"token hvariable\">unHtmlT</span> <span class=\"token operator\">::</span> <span class=\"token constant\">ReaderT</span> <span class=\"token constant\">HtmlEnv</span> <span class=\"token hvariable\">m</span> <span class=\"token hvariable\">a</span><span class=\"token punctuation\">}</span></code></pre>"
    when (s > 0) do
      unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">type</span> <span class=\"token constant\">Html</span> <span class=\"token operator\">=</span> <span class=\"token constant\">HtmlT</span> <span class=\"token constant\">IO</span>\n\
\</code></pre>"
    when (s > 1) do
      unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">data</span> <span class=\"token constant\">HtmlEnv</span> <span class=\"token operator\">=</span> <span class=\"token constant\">HtmlEnv</span>\n\
\  <span class=\"token punctuation\">{</span> <span class=\"token hvariable\">html_current_element</span> <span class=\"token operator\">::</span> <span class=\"token constant\">DOMElement</span>\n\
\  <span class=\"token punctuation\">,</span> <span class=\"token hvariable\">html_insert_before_anchor</span> <span class=\"token operator\">::</span> <span class=\"token constant\">Maybe</span> <span class=\"token constant\">DOMNode</span>\n\
\  <span class=\"token punctuation\">,</span> <span class=\"token hvariable\">html_reactive_env</span> <span class=\"token operator\">::</span> <span class=\"token constant\">ReactiveEnv</span>\n\
\  <span class=\"token punctuation\">}</span></code></pre>"
    when (s > 2) do
      unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">data</span> <span class=\"token constant\">ReactiveEnv</span> <span class=\"token operator\">=</span> <span class=\"token constant\">ReactiveEnv</span>\n\
\  <span class=\"token punctuation\">{</span> <span class=\"token hvariable\">renv_subscriptions</span> <span class=\"token operator\">::</span> <span class=\"token constant\">IORef</span> <span class=\"token punctuation\">(</span><span class=\"token constant\">M<span class=\"token punctuation\">.</span>Map</span> <span class=\"token constant\">EventId</span> <span class=\"token punctuation\">[</span><span class=\"token punctuation\">(</span><span class=\"token constant\">SubscriptionId</span><span class=\"token punctuation\">,</span> <span class=\"token constant\">Callback</span> <span class=\"token constant\">Any</span><span class=\"token punctuation\">)</span><span class=\"token punctuation\">]</span><span class=\"token punctuation\">)</span>\n\
\  <span class=\"token punctuation\">,</span> <span class=\"token hvariable\">renv_finalizers</span> <span class=\"token operator\">::</span> <span class=\"token constant\">IORef</span> <span class=\"token punctuation\">[</span><span class=\"token constant\">Canceller</span><span class=\"token punctuation\">]</span>\n\
\  <span class=\"token punctuation\">,</span> <span class=\"token hvariable\">renv_id_generator</span> <span class=\"token operator\">::</span> <span class=\"token constant\">IORef</span> <span class=\"token constant\">Int</span>\n\
\  <span class=\"token punctuation\">}</span></code></pre>"
  (4, s) -> do
    h1_ "Event, Dynamic and DynRef"
    div_ [class_ "Slide-4-eventWrapper"] do
      eventVersionRef <- newRef @Int 2
      button_ do
        dynText $ T.pack . show <$> fromRef eventVersionRef
        on_ "click" $ modifySync eventVersionRef nextEventVersion
      dyn $ fromRef eventVersionRef <&> \case
        1 -> unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">newtype</span> <span class=\"token constant\">Event</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">=</span> <span class=\"token constant\">Event</span>\n\
  \  <span class=\"token punctuation\">{</span> <span class=\"token hvariable\">unEvent</span> <span class=\"token operator\">::</span> <span class=\"token punctuation\">(</span><span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token constant\">IO</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span><span class=\"token punctuation\">)</span> <span class=\"token operator\">-&gt;</span> <span class=\"token constant\">IO</span> <span class=\"token punctuation\">(</span><span class=\"token constant\">IO</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span><span class=\"token punctuation\">)</span>\n\
  \  <span class=\"token punctuation\">}</span></code></pre>"
        _ -> unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">newtype</span> <span class=\"token constant\">Event</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">=</span> <span class=\"token constant\">Event</span>\n\
  \  <span class=\"token punctuation\">{</span> <span class=\"token hvariable\">unEvent</span> <span class=\"token operator\">::</span> <span class=\"token constant\">ReactiveEnv</span> <span class=\"token operator\">-&gt;</span> <span class=\"token constant\">Callback</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token constant\">IO</span> <span class=\"token constant\">Canceller</span>\n\
  \  <span class=\"token punctuation\">}</span></code></pre>"
    unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">data</span> <span class=\"token constant\">Dynamic</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">=</span> <span class=\"token constant\">Dynamic</span>\n\
\  <span class=\"token punctuation\">{</span> <span class=\"token hvariable\">dynamic_read</span> <span class=\"token operator\">::</span> <span class=\"token constant\">IO</span> <span class=\"token hvariable\">a</span>\n\
\  <span class=\"token punctuation\">,</span> <span class=\"token hvariable\">dynamic_updates</span> <span class=\"token operator\">::</span> <span class=\"token constant\">Event</span> <span class=\"token hvariable\">a</span>\n\
\  <span class=\"token punctuation\">}</span></code></pre>"
    unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">data</span> <span class=\"token constant\">DynRef</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">=</span> <span class=\"token constant\">DynRef</span>\n\
\  <span class=\"token punctuation\">{</span> <span class=\"token hvariable\">dynref_dynamic</span> <span class=\"token operator\">::</span> <span class=\"token constant\">Dynamic</span> <span class=\"token hvariable\">a</span>\n\
\  <span class=\"token punctuation\">,</span> <span class=\"token hvariable\">dynref_modifier</span> <span class=\"token operator\">::</span> <span class=\"token constant\">Modifier</span> <span class=\"token hvariable\">a</span> <span class=\"token comment\">-- (a -&gt; a) -&gt; Transact ()</span>\n\
\  <span class=\"token punctuation\">}</span></code></pre>"
    unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">type</span> <span class=\"token constant\">Callback</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">=</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token constant\">Transact</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span>\n\
\<span class=\"token keyword\">type</span> <span class=\"token constant\">Trigger</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">=</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token constant\">Transact</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span>\n\
\<span class=\"token keyword\">type</span> <span class=\"token constant\">Modifier</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">=</span> <span class=\"token punctuation\">(</span><span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">a</span><span class=\"token punctuation\">)</span> <span class=\"token operator\">-&gt;</span> <span class=\"token constant\">Transact</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span>\n\
\<span class=\"token keyword\">type</span> <span class=\"token constant\">Canceller</span> <span class=\"token operator\">=</span> <span class=\"token constant\">IO</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span></code></pre>"
  (5, s) -> do
    h1_ $ unsafeHtml "What is <code>DynRef</code>?"
    unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">data</span> <span class=\"token constant\">DynRef</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">=</span> <span class=\"token constant\">DynRef</span>\n\
\  <span class=\"token punctuation\">{</span> <span class=\"token hvariable\">dynref_dynamic</span> <span class=\"token operator\">::</span> <span class=\"token constant\">Dynamic</span> <span class=\"token hvariable\">a</span>\n\
\  <span class=\"token punctuation\">,</span> <span class=\"token hvariable\">dynref_modifier</span> <span class=\"token operator\">::</span> <span class=\"token constant\">Modifier</span> <span class=\"token hvariable\">a</span> <span class=\"token comment\">-- (a -&gt; a) -&gt; Transact ()</span>\n\
\  <span class=\"token punctuation\">}</span></code></pre>"
    when (s > 0) do
      unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token hvariable\">newRef</span> <span class=\"token operator\">::</span> <span class=\"token hvariable\">forall</span> <span class=\"token hvariable\">a</span> <span class=\"token hvariable\">m</span><span class=\"token punctuation\">.</span> <span class=\"token constant\">MonadReactive</span> <span class=\"token hvariable\">m</span> <span class=\"token operator\">=&gt;</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">m</span> <span class=\"token punctuation\">(</span><span class=\"token constant\">DynRef</span> <span class=\"token hvariable\">a</span><span class=\"token punctuation\">)</span>\n\
\</code></pre>"
    when (s > 1) do
      unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token hvariable\">readRef</span> <span class=\"token operator\">::</span> <span class=\"token constant\">MonadIO</span> <span class=\"token hvariable\">m</span> <span class=\"token operator\">=&gt;</span> <span class=\"token constant\">DynRef</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">m</span> <span class=\"token hvariable\">a</span>\n\
\<span class=\"token hvariable\">readsRef</span> <span class=\"token operator\">::</span> <span class=\"token constant\">MonadIO</span> <span class=\"token hvariable\">m</span> <span class=\"token operator\">=&gt;</span> <span class=\"token punctuation\">(</span><span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">b</span><span class=\"token punctuation\">)</span> <span class=\"token operator\">-&gt;</span> <span class=\"token constant\">DynRef</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">m</span> <span class=\"token hvariable\">b</span>\n\
\<span class=\"token hvariable\">writeRef</span> <span class=\"token operator\">::</span> <span class=\"token constant\">MonadIO</span> <span class=\"token hvariable\">m</span> <span class=\"token operator\">=&gt;</span> <span class=\"token constant\">DynRef</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">m</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span>\n\
\<span class=\"token hvariable\">writeSync</span> <span class=\"token operator\">::</span> <span class=\"token constant\">DynRef</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token constant\">Transact</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span>\n\
\<span class=\"token hvariable\">modifyRef</span> <span class=\"token operator\">::</span> <span class=\"token constant\">MonadIO</span> <span class=\"token hvariable\">m</span> <span class=\"token operator\">=&gt;</span> <span class=\"token constant\">DynRef</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token punctuation\">(</span><span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">a</span><span class=\"token punctuation\">)</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">m</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span>\n\
\<span class=\"token hvariable\">modifySync</span> <span class=\"token operator\">::</span> <span class=\"token constant\">DynRef</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token punctuation\">(</span><span class=\"token hvariable\">a</span> <span class=\"token operator\">-&gt;</span> <span class=\"token hvariable\">a</span><span class=\"token punctuation\">)</span> <span class=\"token operator\">-&gt;</span> <span class=\"token constant\">Transact</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">)</span></code></pre>"
  (6, s) -> do
    h1_ "What is Transact?"
    unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">newtype</span> <span class=\"token constant\">Transact</span> <span class=\"token hvariable\">a</span> <span class=\"token operator\">=</span> <span class=\"token constant\">Transact</span>\n\
\  <span class=\"token punctuation\">{</span> <span class=\"token hvariable\">unTransact</span> <span class=\"token operator\">::</span> <span class=\"token constant\">StateT</span> <span class=\"token constant\">TransactState</span> <span class=\"token constant\">IO</span> <span class=\"token hvariable\">a</span>\n\
\  <span class=\"token punctuation\">}</span></code></pre>"
    when (s > 0) do
      unsafeHtml "<pre class=\"language-haskell\" tabindex=\"0\"><code class=\"language-haskell\"><span class=\"token keyword\">let</span> <span class=\"token hvariable\">abDyn</span> <span class=\"token operator\">=</span> <span class=\"token punctuation\">(</span><span class=\"token punctuation\">,</span><span class=\"token punctuation\">)</span> <span class=\"token operator\">&lt;$&gt;</span> <span class=\"token hvariable\">aDyn</span> <span class=\"token operator\">&lt;*&gt;</span> <span class=\"token hvariable\">bDyn</span></code></pre>"
  (7, s) -> do
    henv <- ask
    h1_ "Game Of Life"
    lifeStateRef <- newRef $ Set.fromList gospelGun
    threadStateRef <- newRef Nothing
    fpsRef <- newRef 0
    div_ [class_ "Content"] do
      div_ [class_ "Life"] do
        forM_ [miny..maxy] \y ->
          forM_ [minx..maxx] \x -> div_ do
            toggleClass "Alive" $ Set.member (x, y) <$> fromRef lifeStateRef
            style_ $ T.pack $ "top: " <> show (y * 4) <> "px; left: " <> show (x * 4) <> "px"
      button_ do
        "Next Gen"
        onOptions "click" defaultListenerOpts {lo_sync_callback = True} $
          const $ modifyRef lifeStateRef $ nextGen bounds
      button_ do
        dynText $ bool "Run" "Stop" . isJust <$> fromRef threadStateRef
        on_ "click" $ handleRun henv threadStateRef lifeStateRef fpsRef
      p_ do
        "Cells alive:"
        dynText $ T.pack . show . Set.size <$> fromRef lifeStateRef
        ",  FPS:"
        dynText $ T.pack . show <$> fromRef fpsRef
  (8, s) -> do
    henv <- ask
    h1_ "Faster Game Of Life"
    lifeStateRef <- newRef $ Set.fromList gospelGun
    threadStateRef <- newRef Nothing
    fpsRef <- newRef 0
    div_ [class_ "Content"] do
      lifeEl <- div_ [class_ "Life"] do
        forM_ [miny..maxy] \y ->
          forM_ [minx..maxx] \x -> div_ do
            style_ $ T.pack $ "top: " <> show (y * 4) <> "px; left: "
              <> show (x * 4) <> "px"
        asks html_current_element
      forDyn_ (fromRef lifeStateRef) $ liftIO . updateDomElements lifeEl
      button_ do
        "Next Gen"
        onOptions "click" defaultListenerOpts {lo_sync_callback = True} $
          const $ modifyRef lifeStateRef $ nextGen bounds
      button_ do
        dynText $ bool "Run" "Stop" . isJust <$> fromRef threadStateRef
        on_ "click" $ handleRun henv threadStateRef lifeStateRef fpsRef
      p_ do
        "Cells alive:"
        dynText $ T.pack . show . Set.size <$> fromRef lifeStateRef
        ",  FPS:"
        dynText $ T.pack . show <$> fromRef fpsRef
  (9, s) -> do
    h1_ [style_ "font-size: 64px; padding-top: 250px"]
      "The End :-)"

  _ -> slidesWidget (0, 0)
  where
    bounds@(minx, maxx, miny, maxy) = (0, 127, 0, 127)
    gospelGun =
      [ (1, 5),(1, 6),(2, 5),(2, 6),(11, 5),(11, 6),(11, 7),(12, 4),(12, 8)
      , (13, 3),(13, 9),(14, 3),(14, 9),(15, 6),(16, 4),(16, 8),(17, 5),(17, 6)
      , (17, 7),(18, 6),(21, 3),(21, 4),(21, 5),(22, 3),(22, 4),(22, 5),(23, 2)
      , (23, 6),(25, 1),(25, 2),(25, 6),(25, 7),(35, 3),(35, 4),(36, 3),(36, 4)
      ]
    shortcuts :: [(Html (), [Html ()])] -> Html ()
    shortcuts = table_ . tbody_ . mapM_ shortcutRow
    shortcutRow (title, keys) = tr_ do
      td_ title
      td_ . sequence_ . L.intersperse (text ", ") . fmap (span_ [class_ "Shortcut"]) $ keys
    externalLinkIcon = "<svg xmlns=\"http://www.w3.org/2000/svg\" x=\"0px\" y=\"0px\"\n\
\width=\"12\" height=\"12\"\n\
\viewBox=\"0 0 30 30\"\n\
\style=\" fill:#0089ff; bottom: -2px; position: relative;\"><path d=\"M 25.980469 2.9902344 A 1.0001 1.0001 0 0 0 25.869141 3 L 20 3 A 1.0001 1.0001 0 1 0 20 5 L 23.585938 5 L 13.292969 15.292969 A 1.0001 1.0001 0 1 0 14.707031 16.707031 L 25 6.4140625 L 25 10 A 1.0001 1.0001 0 1 0 27 10 L 27 4.1269531 A 1.0001 1.0001 0 0 0 25.980469 2.9902344 z M 6 7 C 4.9069372 7 4 7.9069372 4 9 L 4 24 C 4 25.093063 4.9069372 26 6 26 L 21 26 C 22.093063 26 23 25.093063 23 24 L 23 14 L 23 11.421875 L 21 13.421875 L 21 16 L 21 24 L 6 24 L 6 9 L 14 9 L 16 9 L 16.578125 9 L 18.578125 7 L 16 7 L 14 7 L 6 7 z\"></path></svg>"
    nextEventVersion = \case 1 -> 2; _ -> 1

slideStepLengths :: [Int]
slideStepLengths = [1, 5, 1, 4, 1, 3, 2, 1, 1, 1]

updateDomElements :: DOMElement -> Set.Set (Int, Int) -> IO ()
updateDomElements lifeEl lifeState = do
  childrenAlife <- js_querySelectorAll lifeEl ".Alive"
  forM_ [0.. JS.length childrenAlife - 1] \idx -> do
    let node = childrenAlife JS.! idx
    js_classListRemove (coerce node) "Alive"
  forM_ lifeState \(x, y) -> do
    let pos = y * 128 + x
    node <- js_children lifeEl pos
    js_classListAdd (coerce node) "Alive"

handleRun
  :: HtmlEnv
  -> DynRef (Maybe ThreadId)
  -> DynRef (Set.Set (Int, Int))
  -> DynRef Double
  -> Transact ()
handleRun henv threadStateRef lifeStateRef fpsRef =
  readRef threadStateRef >>= \case
    Just threadId -> do
      liftIO $ killThread threadId
      writeRef threadStateRef Nothing
    Nothing -> do
      let
        go t0 = do
          modifyRef lifeStateRef $ nextGen bounds
          t1 <- liftIO js_performanceNow
          writeRef fpsRef $ fromIntegral (round ((1000.0 / (t1 - t0)) * 100)) / 100
          threadDelay 0
          go t1
      t <- liftIO js_performanceNow
      threadId <- liftIO $ execHtmlT henv $ forkHtml (go t)
      writeRef threadStateRef (Just threadId)
  where
    bounds@(minx, maxx, miny, maxy) = (0, 127, 0, 127)

slidesStyles :: Text
slidesStyles = "\
  \.Slide h1 {\
  \  margin: 0;\
  \  color: rgba(0,0,0,0.87);\
  \  font-weight: 400;\
  \  font-size: 35px;\
  \  text-align: center;\
  \}\
  \.BulletedList {\
  \  list-style: none;\
  \  padding: 0;\
  \  font-size: 24px;\
  \}\
  \.BulletedList li:before {\
  \  content: \"➢\";\
  \  margin: 0 12px;\
  \  font-size: 1.3em;\
  \}\
  \.Life {\
  \  width: 510px;\
  \  height: 510px;\
  \  position: relative;\
  \}\
  \.Life > div {\
  \  width: 3px;\
  \  height: 3px;\
  \  position: absolute;\
  \  background: rgba(0,0,0,0.08);\
  \}\
  \.Life > div.Alive {\
  \  background: black;\
  \}\
  \.Slide-1 h1 {\
  \  margin-top: 24px;\
  \  text-align: center;\
  \  font-size: 36px;\
  \  font-weight: 400;\
  \}\
  \.Slide-1 h2 {\
  \  margin: 24px 0 0 0;\
  \  text-align: center;\
  \  font-weight: 400;\
  \}\
  \.Slide-1-links {\
  \  position: absolute;\
  \  right: 24px;\
  \  bottom: 16px;\
  \  text-align: right;\
  \}\
  \.Slide-1-link {\
  \  font-size: 12px;\
  \}\
  \.Slide-1-legend {\
  \  position: absolute;\
  \  left: 24px;\
  \  bottom: 16px;\
  \  line-height: 1.5em;\
  \  font-size: 14px;\
  \}\
  \.Slide-1-legend table td:nth-child(1) {\
  \  text-align: right;\
  \  padding-right: 16px;\
  \}\
  \.Shortcut {\
  \  border: solid 1px rgba(0,0,0,0.2);\
  \  border-radius: 3px;\
  \  padding: 2px 6px;\
  \  min-width: 18px;\
  \  text-align: center;\
  \  font-family: monospace;\
  \}\
  \.Slide-1-contents {\
  \  list-style: decimal;\
  \}\
  \.Slide-1-contents a {\
  \  line-height: 1.4em;\
  \}\
  \.Slide2-Background {\
  \  right: 24px;\
  \  top: 110px;\
  \  position: absolute;\
  \}\
  \.Slide2-Background img {\
  \  filter: opacity(0.6);\
  \}\
  \a:link {\
  \    color: #0089ff;\
  \    text-decoration: none;\
  \    border-bottom: 1px solid rgba(0,137,255,.3);\
  \    -webkit-transition: color .3s ease,border-color .3s ease;\
  \    transition: color .3s ease,border-color .3s ease\
  \}\
  \a:visited {\
  \    color: #b40eb4;\
  \    border-color: rgba(180,14,180,.3)\
  \}\
  \a:link:hover,a:visited:hover {\
  \    color: #f41224;\
  \    border-color: rgba(244,18,36,.3)\
  \}\
  \.no_visited a:visited,a.no_visited:visited {\
  \    color: #0089ff;\
  \    border-color: rgba(0,137,255,.3)\
  \}\
  \.no_visited a:visited:hover,a.no_visited:visited:hover {\
  \    color: #f41224;\
  \    border-color: rgba(244,18,36,.3)\
  \}\
  \.border-red {\
  \  border-color: red;\
  \}\
  \.wrapper > *:nth-child(1) {\
  \  margin-bottom: 8px;\
  \}\
  \.wrapper > *:nth-child(4) {\
  \  margin-left: 8px;\
  \}\
  \.Slide-4-eventWrapper {\
  \  position: relative;\
  \}\
  \.Slide-4-eventWrapper button {\
  \  position: absolute;\
  \  bottom: 8px;\
  \  right: 8px;\
  \}"
