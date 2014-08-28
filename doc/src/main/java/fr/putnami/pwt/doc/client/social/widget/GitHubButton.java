package fr.putnami.pwt.doc.client.social.widget;

import fr.putnami.pwt.doc.client.social.base.AbstractSocialButton;

public class GitHubButton extends AbstractSocialButton {

	public GitHubButton() {
		setIconType(CONSTANT.githubIcon());
		setTitle(CONSTANT.githubPopover());
		setSocialUtlTemplate(CONSTANT.githubUrl());
		setAsPopup(false);
	}
}
