package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.core.theme.client.CssStyle;
import fr.putnami.pwt.core.widget.client.ButtonGroup;
import fr.putnami.pwt.core.widget.client.base.SimpleStyle;
import fr.putnami.pwt.core.widget.client.util.StyleUtils;

public class SocialBar extends ButtonGroup {

	private static final CssStyle STYLE_SOCIAL_BAR = new SimpleStyle("social-bar");

	public SocialBar() {
		StyleUtils.addStyle(this, SocialBar.STYLE_SOCIAL_BAR);
		this.add(new TwitterButton());
		this.add(new FacebookButton());
		this.add(new GooglePlusButton());
		this.add(new LinkedInButton());
		this.add(new GitHubButton());
	}

}
