/**
 * This file is part of pwt.
 *
 * pwt is free software: you can redistribute it and/or modify it under the terms of the GNU Lesser
 * General Public License as published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * pwt is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
 * implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Lesser
 * General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with pwt. If not,
 * see <http://www.gnu.org/licenses/>.
 */
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
