package fr.putnami.pwt.doc.client.social.constant;

import com.google.gwt.i18n.client.Constants;

public interface SocialConstants extends Constants {

	@DefaultStringValue("http://pwt.putnami.org")
	String pwtUrl();

	@DefaultStringValue("twitter-squared")
	String twitterIcon();

	@DefaultStringValue("https://twitter.com/share?url={0}&text={1}&via=PutnamiTeam")
	String twitterUrl();

	@DefaultStringValue("Share on Twitter")
	String twitterPopover();

	@DefaultStringValue("facebook-squared")
	String facebookIcon();

	@DefaultStringValue("https://www.facebook.com/sharer.php?u={0}&t={1}")
	String facebookUrl();

	@DefaultStringValue("Share on Facebook")
	String facebookPopover();

	@DefaultStringValue("linkedin-squared")
	String linkedinIcon();

	@DefaultStringValue("https://www.linkedin.com/shareArticle?mini=true&url={0}&title={1}")
	String linkedinUrl();

	@DefaultStringValue("Share on LinkedIn")
	String linkedinPopover();

	@DefaultStringValue("gplus-squared")
	String gplusIcon();

	@DefaultStringValue("https://plus.google.com/share?url={0}")
	String gplusUrl();

	@DefaultStringValue("Share on Google+")
	String gplusPopover();

	@DefaultStringValue("github-squared")
	String githubIcon();

	@DefaultStringValue("https://github.com/Putnami/putnami-pwt")
	String githubUrl();

	@DefaultStringValue("View on GitHub")
	String githubPopover();

}
