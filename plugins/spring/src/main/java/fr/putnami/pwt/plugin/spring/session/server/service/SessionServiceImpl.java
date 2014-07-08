package fr.putnami.pwt.plugin.spring.session.server.service;

import javax.servlet.http.HttpSession;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.authentication.AnonymousAuthenticationToken;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.authority.AuthorityUtils;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.web.authentication.UsernamePasswordAuthenticationFilter;
import org.springframework.security.web.authentication.WebAuthenticationDetails;
import org.springframework.security.web.util.TextEscapeUtils;
import org.springframework.stereotype.Service;

import fr.putnami.pwt.core.security.shared.constant.SecurityConstants;
import fr.putnami.pwt.core.security.shared.domain.SessionDto;
import fr.putnami.pwt.core.security.shared.domain.SignInRequest;
import fr.putnami.pwt.core.security.shared.exception.SignFailledException;
import fr.putnami.pwt.core.security.shared.service.SessionService;
import fr.putnami.pwt.plugin.spring.rpc.server.util.RequestThreadLocalUtils;

@Service
public class SessionServiceImpl implements SessionService {

	@Autowired
	private AuthenticationManager authenticationManager;

	@Override
	public SessionDto getCurrentSession() {
		SessionDto session = new SessionDto();

		Authentication authentication = SecurityContextHolder.getContext().getAuthentication();
		Object principal = authentication.getPrincipal();

		if (principal instanceof UserDetails) {
			UserDetails userDetails = (UserDetails) principal;
			session.setUsername(userDetails.getUsername());
		}
		else {
			session.setUsername(principal.toString());
		}

		session.getRoles().addAll(AuthorityUtils.authorityListToSet(authentication.getAuthorities()));

		return session;
	}

	@Override
	public SessionDto signIn(SignInRequest request) throws SignFailledException {
		try {
			UsernamePasswordAuthenticationToken authRequest =
					new UsernamePasswordAuthenticationToken(request.getUsername(), request.getPassword());
			HttpSession session = RequestThreadLocalUtils.getSession();
			session.setAttribute(UsernamePasswordAuthenticationFilter.SPRING_SECURITY_LAST_USERNAME_KEY,
					TextEscapeUtils.escapeEntities(request.getUsername()));

			authRequest.setDetails(new WebAuthenticationDetails(RequestThreadLocalUtils.getRequest()));

			Authentication authResponse = authenticationManager.authenticate(authRequest);
			SecurityContextHolder.getContext().setAuthentication(authResponse);

			return getCurrentSession();
		}
		catch (AuthenticationException e) {
			throw new SignFailledException(SecurityConstants.BAD_CREDENTIALS);
		}
	}

	@Override
	public SessionDto signOut() {
		RequestThreadLocalUtils.getSession().invalidate();
		SecurityContextHolder.clearContext();

		AnonymousAuthenticationToken authRequest = new AnonymousAuthenticationToken(
				SecurityConstants.USER_ANONYMOUS, SecurityConstants.USER_ANONYMOUS,
				AuthorityUtils.createAuthorityList(SecurityConstants.ROLE_ANONYMOUS));

		authRequest.setDetails(new WebAuthenticationDetails(RequestThreadLocalUtils.getRequest()));

		SecurityContextHolder.getContext().setAuthentication(authRequest);

		return getCurrentSession();
	}

}
