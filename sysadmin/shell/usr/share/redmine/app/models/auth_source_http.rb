class AuthSourceHTTP < AuthSource
    def authenticate(login, password)
        logger.debug "AuthSourceHTTP.authenticate(#{login}, #{password})"

        return nil if login.nil? || login.blank?

        i = login.index("@")
        if i
            firstname = login[0, i]
            mail = firstname + login[i, login.length].downcase
        else
            firstname = login
            mail = nil
        end

        #logger.debug "AuthSourceHTTP.authenticate(): #{firstname}, #{mail}"
        #logger.debug "AuthSourceHTTP.authenticate(): onthefly_register=#{onthefly_register}"

        if (onthefly_register?)
            return {
                :firstname      => firstname,
                :lastname       => "lastname",
                :mail           => mail,
                :auth_source_id => self.id
            }
        else
            return nil
        end
    end

    def auth_method_name
        "HTTP"
    end
end
